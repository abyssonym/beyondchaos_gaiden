from randomtools.utils import map_to_snes, map_from_snes
from randomtools.tablereader import (
    tblpath, write_patch, set_addressing_mode, get_open_file, verify_patchlist)

import json
from os import path


TEXT_TABLE_FILEPATH = path.join(tblpath, 'shorttext.txt')
CHARACTER_MAP = {}
with open(TEXT_TABLE_FILEPATH) as f:
    for line in f:
        character, code = line.split()
        code = int(code, 0x10)
        CHARACTER_MAP[code] = character
        CHARACTER_MAP[character] = code

for (a, b) in {
        ' ': 0xff,
        '\n': 0x01,
        }.items():
    CHARACTER_MAP[a] = b
    CHARACTER_MAP[b] = a


def map_character(c):
    old_c = c
    try:
        c = CHARACTER_MAP[c]
    except KeyError:
        c = '<{0:0>2x}>'.format(c)
    if c == '\n':
        c = '<01>'
    if isinstance(c, int):
        c = bytes([c])
    return c

def map_text(text):
    if isinstance(text, bytes):
        result = ''
    else:
        assert isinstance(text, str)
        result = b''
    while text:
        if text[0] == '<':
            assert text[3] == '>'
            c = bytes([int(text[1:3], 0x10)])
            result += c
            text = text[4:]
        else:
            c = map_character(text[0])
            text = text[1:]
            result += c
    return result


def populate_data(data, filename, address):
    f = get_open_file(filename)

    mindex = min(data)
    maxdex = max(data)
    assert mindex >= 0
    pointer = address + (maxdex * 2) + 2
    nothing_pointer = pointer
    f.seek(nothing_pointer)
    f.write(b'\x00')
    pointer += 1
    pointers = []
    for i in range(maxdex+1):
        if i not in data:
            values = []
        else:
            values = sorted(data[i])
        if not values:
            pointers.append(nothing_pointer)
            continue
        assert 0 not in values
        values = sorted(set(values))
        values.append(0)
        f.seek(pointer)
        verify_free_space = f.read(len(values))
        assert set(verify_free_space) == {0}
        f.seek(pointer)
        f.write(bytes(values))
        pointers.append(pointer)
        pointer += len(values)

    assert len(pointers) == maxdex + 1
    assert len({map_to_snes(pointer) >> 16 for pointer in pointers}) == 1
    f.seek(address-1)
    verify_free_space = f.read(3)
    assert verify_free_space == b'\x00\x00\x00'
    f.seek(address)

    for pointer in pointers:
        p = pointer & 0xFFFF
        assert 0 <= p <= 0xFFFF
        f.write(p.to_bytes(2, byteorder='little'))


class JunctionManager:
    MAX_LINE_LENGTH = 32

    def __init__(self, outfile, manifest=None, data=None):
        self.outfile = get_open_file(outfile)
        self.directory = tblpath
        self.patches = set()

        assert manifest or data
        if manifest:
            filename = path.join(self.directory, manifest)
            with open(filename) as f:
                full_data = json.loads(f.read())
        else:
            full_data = {}

        if data is not None:
            for key in sorted(data):
                value = data[key]
                if key not in full_data:
                    full_data[key] = value
                if isinstance(value, dict):
                    for key2 in value:
                        full_data[key][key2] = value[key2]
                else:
                    full_data[key] = value

        for key in full_data:
            assert not hasattr(self, key)
            value = full_data[key]
            if isinstance(value, dict) and key.endswith('list'):
                category = key.split('_')[0]
                if category in ('character', 'esper', 'equip',
                                'status', 'monster'):
                    category_cleaner = lambda index: (
                        self.get_category_index(category, index))
                    new_dict = {}
                    for k, v in sorted(value.items()):
                        k = category_cleaner(k)
                        assert k not in new_dict
                        new_dict[k] = v
                    value = new_dict
                    full_data[key] = value

            if key == 'battle_messages':
                value = {int(k, 0x10): v for (k, v) in value.items()}
                full_data[key] = value

            if key in ('patch_list', 'junction_short_names'):
                new_dict = {}
                for k, v in sorted(value.items()):
                    k = self.get_junction_index(k)
                    assert k not in new_dict
                    new_dict[k] = v
                value = new_dict
                full_data[key] = value

            if isinstance(value, dict):
                if key !='junction_indexes':
                    assert all(isinstance(k, int) for k in value.keys())

            if key.endswith('_address') or key.startswith('num_'):
                full_data[key] = self.clean_number(full_data[key])
            setattr(self, key, full_data[key])

        for patch in self.core_patch_list:
            self.patches.add(patch)

    def clean_number(self, value):
        if isinstance(value, int):
            return value
        return int(value, 0x10)

    def add_junction(self, key, junction_item, list_color='whitelist',
                     force_category=None, remove=False):
        junction_index = self.get_junction_index(junction_item)
        if key is None:
            assert list_color == 'whitelist'
            self.always_whitelist = [self.get_junction_index(j)
                                     for j in self.always_whitelist]
            if junction_index not in self.always_whitelist:
                self.always_whitelist.append(junction_index)
            return

        for category in ['character', 'esper', 'equip',
                         'status', 'monster']:
            if force_category and category != force_category:
                continue

            category_cleaner = lambda index: self.get_category_index(category,
                                                                     index)
            try:
                k = category_cleaner(key)
            except (ValueError, AttributeError):
                continue

            attr = '%s_%s' % (category, list_color)
            data = getattr(self, attr)
            if k not in data:
                data[k] = []
            data[k] = [self.get_junction_index(j) for j in data[k]]
            if junction_index not in data[k] and not remove:
                data[k].append(junction_index)
            elif remove and junction_index in data[k]:
                junction_index = self.get_junction_index(junction_index)
                items = [ji for ji in data[k] if
                         self.get_junction_index(ji) != junction_index]
                data[k] = items
            setattr(self, attr, data)
            break
        else:
            raise Exception('Could not %s %s for %s.' %
                            (list_color, junction_item, key))

    def remove_junction(self, key, junction_item, list_color='whitelist',
                        force_category=None):
        self.add_junction(key, junction_item, list_color, force_category,
                          remove=True)

    def add_patch(self, junction_item):
        junction_index = self.get_junction_index(junction_item)
        if junction_index in self.patch_list:
            for patch in self.patch_list[junction_index]:
                self.patches.add(patch)

    def get_junction_index(self, junction_item):
        if junction_item in self.junction_indexes:
            index = self.clean_number(self.junction_indexes[junction_item])
        else:
            index = self.clean_number(junction_item)
        assert 1 <= index <= 0xff
        return index

    def get_category_index(self, category, key):
        if isinstance(key, int):
            return key
        names = [n.lower() for n in getattr(self, '%s_names' % category)]
        key = key.lower()
        if key in names:
            return names.index(key)
        return self.clean_number(key)

    def populate_always(self):
        data = {0: []}
        for junction_item in self.always_whitelist:
            self.add_patch(junction_item)
            data[0].append(self.get_junction_index(junction_item))
        data[0] = sorted(set(data[0]))

        address = self.clean_number(self.always_whitelist_address)
        populate_data(data, self.outfile, address)

    def populate_list(self, category, color, max_index):
        data = {}
        import_data = getattr(self, '%s_%slist' % (category, color))
        category_cleaner = lambda index: self.get_category_index(category,
                                                                 index)
        for key, junction_items in import_data.items():
            key = category_cleaner(key)
            junction_items = [self.get_junction_index(junction_item)
                              for junction_item in junction_items]
            if color != 'black':
                for ji in junction_items:
                    self.add_patch(ji)
            assert key not in data
            data[key] = sorted(set(junction_items))

        if data:
            assert max(data) <= max_index
        if max_index not in data:
            data[max_index] = []

        address = getattr(self, '%s_%slist_address' % (category, color))
        address = self.clean_number(address)
        populate_data(data, self.outfile, address)

    def populate_everything(self):
        self.populate_always()
        self.populate_list('character', 'white', 0xf)
        self.populate_list('character', 'black', 0xf)
        self.populate_list('esper', 'white', 0x1f)
        self.populate_list('esper', 'black', 0x1f)
        self.populate_list('equip', 'white', 0xff)
        self.populate_list('equip', 'black', 0xff)
        self.populate_list('status', 'white', 0x1f)
        self.populate_list('status', 'black', 0x1f)
        self.populate_list('monster', 'white', 0x1ff)
        self.populate_list('monster', 'black', 0x1ff)

    def write_patches(self):
        set_addressing_mode('hirom')
        for patch in sorted(self.patches):
            write_patch(self.outfile, patch)

    def read_descriptions(self, address, num_messages):
        self.outfile.seek(address)
        address_str = self.outfile.read(18)
        offsets_low = int.from_bytes(address_str[1:3], byteorder='little')
        origin_low = int.from_bytes(address_str[6:8], byteorder='little')
        offsets_high = address_str[0xb]
        origin_high = address_str[0xf]
        a = address_str[0:1]
        b = address_str[3:6]
        c = address_str[8:0xb]
        d = address_str[0xc:0xf]
        e = address_str[0x10:]
        validation = b'\xa2\x86\xe7\xa2\x86\xeb\xa9\x85\xe9\xa9\x85\xed'
        assert a + b + c + d + e == validation

        offsets_addr = map_from_snes(offsets_low | (offsets_high << 16))
        origin_addr = map_from_snes(origin_low | (origin_high << 16))

        self.outfile.seek(offsets_addr)
        messages = []
        for i in range(num_messages):
            self.outfile.seek(offsets_addr + (i*2))
            offset = int.from_bytes(self.outfile.read(2), byteorder='little')
            self.outfile.seek(origin_addr + offset)
            message = b''
            while True:
                c = self.outfile.read(1)
                if c == b'\x00':
                    break
                message += c
            message = map_text(message)
            messages.append(message)

        return messages

    def write_descriptions(self, address, messages, offsets_addr):
        offsets_addr = map_to_snes(offsets_addr)
        origin_addr = offsets_addr
        a, b, c, d, e = (b'\xa2', b'\x86\xe7\xa2', b'\x86\xeb\xa9',
                         b'\x85\xe9\xa9', b'\x85\xed')
        offsets_low = int.to_bytes(offsets_addr & 0xFFFF,
                                   byteorder='little', length=2)
        origin_low = int.to_bytes(origin_addr & 0xFFFF,
                                  byteorder='little', length=2)
        offsets_high = bytes([offsets_addr >> 16])
        origin_high = bytes([origin_addr >> 16])
        address_str = (a + offsets_low + b + origin_low +
                       c + offsets_high + d + origin_high + e)
        self.outfile.seek(address)
        self.outfile.write(address_str)

        message_pointer = offsets_addr + (len(messages) * 2)
        first_offset = message_pointer
        offset_cache = {}
        for i, message in enumerate(messages):
            message = message.replace('<01>', '\n')
            assert message.count('\n') <= 1
            assert all(len(line) <= self.MAX_LINE_LENGTH
                       for line in message.split('\n'))
            m = map_text(message) + b'\x00'
            if m in offset_cache:
                offset = offset_cache[m]
            else:
                assert message_pointer & 0xFFFF >= first_offset & 0xFFFF
                offset = (message_pointer-offsets_addr) & 0xFFFF
                self.outfile.seek(map_from_snes(message_pointer))
                self.outfile.write(m)
                message_pointer += len(m)
                offset_cache[m] = offset
            self.outfile.seek(map_from_snes(offsets_addr + (i*2)))
            self.outfile.write(offset.to_bytes(byteorder='little', length=2))

    def generate_descriptions(self, category, address=None,
                              num_messages=None):
        if address is None and num_messages is None:
            if category == 'equip':
                address = self.equip_description_address
                num_messages = self.num_equip_descriptions
            elif category == 'esper':
                address = self.esper_description_address
                num_messages = self.num_esper_descriptions

        messages = self.read_descriptions(address, num_messages)
        whitelist = getattr(self, '%s_whitelist' % category)
        for key, values in sorted(whitelist.items()):
            if not values:
                continue
            s = 'J:'
            for junction_index in values:
                append = ' %s,' % self.junction_short_names[junction_index]
                test = s + append
                if len(test.split('\n')[-1]) > self.MAX_LINE_LENGTH:
                    append = '\n' + append[1:]
                s = s + append
            message = '\n'.join(s.split('\n')[:2])
            assert message[-1] == ','
            if len(message) < len(s):
                message = message[:-1] + '…'
            else:
                message = message[:-1]
            messages[key] = message
        return messages

    def rewrite_descriptions(self):
        messages = self.generate_descriptions('equip')
        self.write_descriptions(self.equip_description_address, messages,
                                self.equip_free_address)
        messages = self.generate_descriptions('esper')
        self.write_descriptions(self.esper_description_address, messages,
                                self.esper_free_address)

    def read_battle_messages(self, num_messages=256, address=0x11f7a0):
        self.outfile.seek(address)
        pointers = []
        for _ in range(num_messages):
            pointer = int.from_bytes(self.outfile.read(2), byteorder='little')
            pointers.append(pointer)
        messages = []
        for p in pointers:
            self.outfile.seek((address & 0xFF8000) | p)
            message = b''
            while True:
                c = self.outfile.read(1)
                if c == b'\x00':
                    break
                message += c
            messages.append(map_text(message))
        return messages

    def write_battle_messages(self, messages, pointers_address=0x11f7a0,
                              messages_address=0x11f000):
        self.outfile.seek(messages_address)
        self.outfile.write(b'\x00')
        message_cache = {'': messages_address}
        next_free_space = messages_address + 1
        for i, m in enumerate(messages):
            if m in message_cache:
                pointer = message_cache[m]
            else:
                pointer = next_free_space
                self.outfile.seek(pointer)
                self.outfile.write(map_text(m) + b'\x00')
                next_free_space += len(m) + 1
                assert next_free_space <= pointers_address
                message_cache[m] = pointer
            self.outfile.seek(pointers_address + (i*2))
            self.outfile.write((pointer & 0xFFFF).to_bytes(length=2,
                                                           byteorder='little'))

    def rewrite_battle_messages(self):
        messages = self.read_battle_messages()
        for index, message in sorted(self.battle_messages.items()):
            messages[index] = message
        self.write_battle_messages(messages)

    def execute(self):
        self.populate_everything()
        self.write_patches()
        self.rewrite_descriptions()
        self.rewrite_battle_messages()

    def verify_patches(self):
        verify_patchlist(self.outfile, sorted(self.patches))


if __name__ == '__main__':
    from sys import argv
    if len(argv) > 2:
        jm = JunctionManager(argv[1], argv[2])
    else:
        jm = JunctionManager('test.smc', 'bcg_junction_manifest.json')
    jm.execute()
