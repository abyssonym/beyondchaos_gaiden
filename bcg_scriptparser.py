from sys import argv

from randomtools.scriptparser import Parser, hexify
from randomtools.utils import fake_yaml as yaml
from randomtools.utils import read_lines_nocomment


class FF6Parser(Parser):
    B0_CONTEXTS =  ('vehicle', 'overworld')
    CHOICE_MESSAGE_OVERRIDES = {
        0x19c: 2,
        0x2cc: 2,
        0x32a: 2,
        0x386: 2,
        0x3d3: 2,
        0x3d4: 2,
        0x621: 2,
        0x69b: 2,
        0x713: 2,
        0x79d: 2,
        0x872: 2,
        0x9bf: 2,
        0x9c0: 2,
        0x9c1: 2,
        0x9c2: 2,
        0xb80: 2,
        0xb89: 2,
        }
    MESSAGE_TEXTS = {}


    def determine_context(self, inst):
        def on_vehicle(i):
            return i.parameters['on_chocobo'] or i.parameters['in_airship']

        if inst.opcode in (0x6a, 0x6b) and inst.context == 'standard':
            if inst.parameters['map_index'] not in (0, 1, 2, 0x1fe, 0x1ff):
                assert not on_vehicle(inst)
                inst.script.context = 'standard'
            elif on_vehicle(inst):
                inst.script.context = 'vehicle'
            else:
                inst.script.context = 'overworld'
        elif inst.opcode in (0xd2, 0xd3):
            assert inst.context in ('vehicle', 'movement', 'overworld')
            if inst.parameters['map_index'] not in (0, 1, 2, 0x1fe, 0x1ff):
                #assert not on_vehicle(inst)
                inst.script.context = 'standard'
            else:
                if not on_vehicle(inst):
                    inst.script.context = 'overworld'
                else:
                    assert inst.context == inst.script.context == 'vehicle'

    def get_next_instruction(self, script):
        inst = super().get_next_instruction(script)
        self.determine_context(inst)
        if inst.opcode == 0xb0 and inst.context in self.B0_CONTEXTS:
            tell = self.data.tell()
            peek = ord(self.data.read(1))
            if peek & 0xf8 == 0xc0:
                inst.script.context = 'standard'
            self.data.seek(tell)
        return inst

    def read_variable_length(self, instruction):
        inst = instruction
        if (inst.context == 'standard' and inst.opcode in (0xc0, 0xc8)) or \
                (inst.context in ('movement', 'overworld', 'vehicle') and
                 inst.opcode in (0xb0, 0xb8)):
            numc = inst.parameters['num_conditions'] + 1
            conditions = []
            for i in range(numc):
                condition = self.data.read(2)
                condition = int.from_bytes(condition, byteorder='little')
                flag = condition & 0x7fff
                value = condition >> 15
                #inst.parameters[f'flag{i}'] = flag
                #inst.parameters[f'value{i}'] = value
                conditions.append((flag, value))
            inst.parameters['conditions'] = conditions
            jump = self.data.read(3)
            jump = int.from_bytes(jump, byteorder='little')
            jump = self.get_tracked_pointer(jump, 0, script=True,
                                            context=inst.context)
            inst.parameters['jump'] = jump
        elif inst.opcode == 0xbe:
            num_cases = inst.parameters['num_cases']
            cases = []
            for i in range(num_cases):
                segment = self.data.read(3)
                value = int.from_bytes(segment, byteorder='little')
                call = value & 0xfffff
                call = self.get_tracked_pointer(call, context=inst.context,
                                                script=True)
                bit = value >> 20
                cases.append((bit, call))
            inst.parameters['cases'] = cases
        elif inst.opcode in (0x73, 0x74):
            data = self.data.read(inst.parameters['width'] *
                                  inst.parameters['height'])
            inst.parameters['data'] = data
        elif inst.opcode == 0xb6:
            #print('WARNING: Opcode 0xb6 behavior depends on '
            #      'preceding message.')
            message_index = None
            for previous_message in inst.script.instructions:
                if 'message' not in previous_message.parameters:
                    continue
                message_index = previous_message.parameters['message']
            #if message_index is None:
            #    import pdb; pdb.set_trace()
            #assert message_index is not None
            if message_index in self.CHOICE_MESSAGE_OVERRIDES:
                num_choices = self.CHOICE_MESSAGE_OVERRIDES[message_index]
            else:
                num_choices = None
            addresses = []
            while True:
                if self.data.tell() in addresses:
                    break
                old_offset = self.data.tell()
                segment = self.data.read(3)
                value = int.from_bytes(
                        segment, byteorder=self.config['byteorder'])
                address = value & 0xfffff
                if address >= 0x40000:
                    self.data.seek(old_offset)
                    break
                if addresses and address & 0xff == 0xfe:
                    if self.data.tell()-2 in addresses:
                        self.data.seek(old_offset)
                        break
                addresses.append(address)
                if num_choices and len(addresses) >= num_choices:
                    break
            if message_index is None and len(addresses) != 2:
                import pdb; pdb.set_trace()
                assert False
            selections = [self.get_tracked_pointer(address, script=True,
                                                   context=inst.context)
                          for address in addresses]
            inst.parameters['selections'] = selections
            return selections
        else:
            msg = (f'ERROR: Unknown variable length opcode: '
                   f'{inst.opcode:0>2x} (inst.context)')
            print(msg)
            import pdb; pdb.set_trace()
            raise Exception(msg)

    def format_parameter(self, instruction, parameter_name):
        opcode, context = instruction.opcode, instruction.context
        formatted = super().format_parameter(instruction, parameter_name)
        if opcode in (0xb0, 0xb8, 0xc0, 0xc8) and \
                parameter_name == 'conditions':
            formatted = formatted.replace('-00', '-NO')
            formatted = formatted.replace('-01', '-YES')
            if opcode & 0x8:
                formatted = formatted.replace('&', ' and ')
            else:
                formatted = formatted.replace('&', ' or ')

        elif opcode == 0xbe and parameter_name == 'cases':
            formatted = formatted.replace('&', ' or ')

        elif opcode == 0xb6 and parameter_name == 'selections':
            formatted = formatted.replace('&', ' or ')

        while '  ' in formatted:
            formatted = formatted.replace('  ', ' ')

        return formatted

    def format_instruction(self, instruction):
        base = str(instruction)
        if instruction.opcode in (0x73, 0x74):
            assert 'data' in instruction.parameters
            assert 'width' in instruction.parameters
            width = instruction.parameters['width']
            lines = base.split('\n')
            formatted = []
            for line in lines:
                if not line.lstrip().startswith('data '):
                    formatted.append(line)
                    continue

                prefix, data = line.split(' = ')
                prefix += ' = '
                assert data == data.strip()
                data = [data[i:i+2] for i in range(0, len(data), 2)]
                data_lines = []
                while data:
                    data_lines.append(' '.join(data[:width]))
                    data = data[width:]
                joiner = '\n' + (' ' * len(prefix))
                data = joiner.join(data_lines)
                line = prefix + data
                formatted.append(line)
            return '\n'.join(formatted)

        if 'message' not in instruction.parameters or \
                instruction.parameters['message'] not in self.MESSAGE_TEXTS:
            return base

        text = self.MESSAGE_TEXTS[instruction.parameters['message']]
        lines = text.split('\n')
        if '|' not in text:
            lines = [f'|{line}|' for line in lines]
        lines = [f'  {line}' for line in lines]
        formatted = base.split('\n')
        formatted = formatted + lines
        formatted = '\n'.join(formatted)
        return formatted

    def interpret_parameter(self, parameter,
                            opcode=None, parameter_name=None, manifest=None,
                            is_list=None):
        if parameter_name in ('conditions', 'cases', 'selections'):
            parameter = parameter.replace('-YES', '-01')
            parameter = parameter.replace('-NO', '-00')
            parameter = parameter.replace('and', '&')
            parameter = parameter.replace('or', '&')
        result = super().interpret_parameter(
                parameter, opcode, parameter_name, manifest, is_list)
        assert result is not None
        return result

    def interpret_instruction(self, line, script=None):
        assert '|' not in line
        prev = None
        if script.instructions:
            prev = script.instructions[-1]
        elif script.joined_before:
            assert script.joined_before.instructions
            prev = script.joined_before.instructions[-1]

        opcode = line
        if '.' in opcode:
            _, opcode = opcode.split('.')
        if ':' in opcode:
            opcode, _ = opcode.split(':')
        if not opcode.startswith('@'):
            opcode = int(opcode.strip(), 0x10)
        if not isinstance(opcode, int):
            opcode = None

        context = script.context

        if context != 'standard' and prev and prev.opcode == 0xb0 \
                and prev.context in self.B0_CONTEXTS:
            if isinstance(opcode, int) and opcode & 0xf8 == 0xc0:
                import pdb; pdb.set_trace()
                script.context = 'standard'

        inst = super().interpret_instruction(line, script)
        if isinstance(inst, self.Instruction):
            self.determine_context(inst)
        return inst

    def variable_instruction_to_bytecode(self, instruction, header):
        opcode, context = instruction.opcode, instruction.context
        data = header
        if opcode in (0x73, 0x74) and context in ('standard',):
            try:
                data += instruction.parameters['data']
            except:
                import pdb; pdb.set_trace()
            return data
        if (context == 'standard' and opcode in (0xc0, 0xc8)) or \
                (context in ('movement', 'overworld', 'vehicle') and
                 opcode in (0xb0, 0xb8)):
            assert len(instruction.parameters['conditions']) == \
                    instruction.parameters['num_conditions'] + 1
            for flag, value in instruction.parameters['conditions']:
                assert value in (0, 1)
                condition = flag | (value << 15)
                data += condition.to_bytes(length=2, byteorder='little')
            jump = instruction.parameters['jump']
            data += jump.converted_smart.to_bytes(length=3, byteorder='little')
            return data
        if (opcode, context) == (0xb6, 'standard'):
            for selection in instruction.parameters['selections']:
                data += selection.converted_smart.to_bytes(length=3,
                                                           byteorder='little')
            return data
        if (opcode, context) == (0xbe, 'standard'):
            assert len(instruction.parameters['cases']) == \
                    instruction.parameters['num_cases']
            for bit, call in instruction.parameters['cases']:
                value = call.converted_smart | (bit << 20)
                data += value.to_bytes(length=3, byteorder='little')
            return data
        raise Exception(f'Unhandled variable instruction - {instruction}')

    def to_bytecode(self):
        bytecode = self.dump_all_scripts()
        return bytecode


if __name__ == '__main__':
    BASE_POINTER = 0xa0000
    romfile = argv[1]
    config = argv[2]
    #pointers = {int(a, 0x10) for a in argv[3:]}
    pointerfile = argv[3]
    pointers = set()
    reserved_contexts = {}
    for line in read_lines_nocomment(pointerfile):
        line = line.strip()
        if not line:
            continue
        context = None
        if ' ' in line:
            while '  ' in line:
                line = line.replace('  ', ' ')
            pointer, context = line.split()
            pointer = int(pointer, 0x10) - BASE_POINTER
            reserved_contexts[pointer] = context
        else:
            pointer = int(line, 0x10) - BASE_POINTER
        pointers.add(pointer)
    with open(romfile, 'r+b') as f:
        f.seek(BASE_POINTER)
        data = f.read()
    parser = FF6Parser(config, data, pointers,
                       reserved_contexts=reserved_contexts)
    parser.format_length = 20

    for (p, script) in sorted(parser.scripts.items()):
        if script.instructions:
            end_address = script.instructions[-1].end_address
            if end_address not in parser.scripts:
                print(f'{end_address:0>5x}')

    #for (p, script) in sorted(parser.scripts.items()):
    #    print(script)
    #    print()
    #parser.reserved_contexts = {}
    #with open('script.import.txt') as f:
    #    parser.import_script(f.read())
    #result = parser.dump_all_scripts()
    #import pdb; pdb.set_trace()
