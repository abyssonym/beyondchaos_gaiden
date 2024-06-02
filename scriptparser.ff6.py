from sys import argv
from randomtools.scriptparser import Parser
from randomtools.utils import read_lines_nocomment, fake_yaml as yaml


CHOICE_MESSAGE_OVERRIDES = {
    0x19c: 2,
    0x3d3: 2,
    0x3d4: 2,
    0xb89: 2,
    }


class FF6Parser(Parser):
    script_contexts = {}

    def set_context(self, script):
        if script.pointer in self.script_contexts:
            context = self.script_contexts[script.pointer]
            if hasattr(script, 'context'):
                assert script.context == context
            script.context = context
        else:
            super().set_context(script)

    def read_variable_length(self, instruction, parameters):
        opcode = instruction.opcode
        if opcode == 0xb6:
            print('WARNING: Opcode 0xb6 behavior depends on '
                  'preceding message.')
            message_index = None
            previous_message = instruction.script.instructions[-2]
            for previous_message in instruction.script.instructions:
                if not previous_message.parameters:
                    continue
                if 'message' not in previous_message.parameters:
                    continue
                message_index = previous_message.parameters['message']
            assert message_index is not None
            if message_index in CHOICE_MESSAGE_OVERRIDES:
                num_choices = CHOICE_MESSAGE_OVERRIDES[message_index]
            else:
                num_choices = None
            addresses = []
            while True:
                if self.data.tell() in addresses:
                    break
                old_offset = self.data.tell()
                segment = self.data.read(3)
                value = int.from_bytes(
                        segment, byteorder=self.config['byte_order'])
                address = value & 0xfffff
                if address >= 0x40000:
                    self.data.seek(old_offset)
                    break
                addresses.append(address)
                if num_choices and len(addresses) >= num_choices:
                    break
            selections = [self.get_tracked_pointer(address)
                          for address in addresses]
            parameters['selections'] = selections
            return selections
        elif opcode == 0xbe:
            num_selections = parameters['num_selections']
            selections = []
            for i in range(num_selections):
                segment = self.data.read(3)
                value = int.from_bytes(
                        segment, byteorder=self.config['byte_order'])
                address = value & 0xfffff
                address = self.get_tracked_pointer(address)
                bit = value >> 20
                selections.append(bit)
                selections.append(address)
            parameters['selections'] = selections
            return selections
        elif opcode in {0x73, 0x74}:
            #segments = []
            #for i in range(parameters['height']):
            #    segment = self.data.read(parameters['width'])
            #    segments.append(segment)
            #parameters['data'] = segments
            #return segments
            data = self.data.read(parameters['width'] * parameters['height'])
            return data
        else:
            raise NotImplementedError

    def get_text(self, value, instruction):
        if instruction.opcode in {0x73, 0x74}:
            width = instruction.parameters['width']
            height = instruction.parameters['height']
            lines = []
            while value:
                line, value = value[:width], value[width:]
                line = ','.join(f'{c:0>2x}' for c in line)
                lines.append(line)
            assert len(lines) == height
            return '\n'.join(lines)
        else:
            raise NotImplementedError

    def get_next_instruction(self, script):
        instruction = super().get_next_instruction(script)
        assert script.context != 'overworld'
        if script.context == 'standard':
            if 0x00 <= instruction.opcode <= 0x34:
                script.context = 'movement'
        elif script.context == 'movement':
            if instruction.opcode in (0xff, 0xfc, 0xfd):
                script.context = 'standard'
        elif script.context == 'overworld':
            if instruction.opcode == 0xff:
                script.context = 'standard'
        elif script.context == 'vehicle':
            if instruction.opcode == 0xff:
                script.context = 'standard'
        else:
            raise Exception(f'Unknown context {script.context}')

        loading_map = False
        if script.context == 'standard':
            if instruction.opcode in {0x6a, 0x6b}:
                loading_map = True
        elif instruction.opcode == 0xd2:
            loading_map = True
        if loading_map:
            parameters = instruction.parameters
            map_index = parameters['map_index']
            script.context = 'standard'
            if map_index in (0, 1, 2, 0x1fe, 0x1ff):
                script.context = 'movement'
                in_airship = parameters['in_airship']
                on_chocobo = parameters['in_airship']
                if in_airship or on_chocobo:
                    script.context = 'vehicle'
                else:
                    #script.context = 'overworld'
                    script.context = 'movement'

        for key, value in instruction.parameters.items():
            if isinstance(value, self.TrackedPointer):
                if value not in self.script_contexts:
                    self.script_contexts[value] = script.context
                assert self.script_contexts[value] == script.context

        return instruction


if __name__ == '__main__':
    BASE_POINTER = 0xa0000
    romfile = argv[1]
    config = argv[2]
    #pointers = {int(a, 0x10) for a in argv[3:]}
    pointerfile = argv[3]
    pointers = set()
    for line in read_lines_nocomment(pointerfile):
        if not line.strip():
            continue
        pointers.add(int(line, 0x10)-BASE_POINTER)
    with open(romfile, 'r+b') as f:
        f.seek(BASE_POINTER)
        data = f.read()
    parser = FF6Parser(config, data, pointers)
