from randomtools.utils import map_to_snes
from randomtools.tablereader import tblpath, write_patch, set_addressing_mode

import json
from os import path

def populate_data(data, filename, address):
    with open(filename, 'r+b') as f:
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
            values.append(0)
            f.seek(pointer)
            f.write(bytes(values))
            pointers.append(pointer)

        assert len(pointers) == maxdex + 1
        assert len({map_to_snes(pointer) >> 16 for pointer in pointers}) == 1
        f.seek(address)
        for pointer in pointers:
            p = pointer & 0xFFFF
            assert 0 <= p <= 0xFFFF
            f.write(p.to_bytes(2, byteorder='little'))


class JunctionManager:
    def __init__(self, outfile, manifest=None, data=None):
        self.outfile = outfile
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
            setattr(self, key, full_data[key])

        for patch in self.core_patch_list:
            self.patches.add(patch)

    def clean_number(self, value):
        if isinstance(value, int):
            return value
        return int(value, 0x10)

    def add_patch(self, junction_item):
        if junction_item in self.patch_list:
            for patch in self.patch_list[junction_item]:
                self.patches.add(patch)

    def get_junction_index(self, junction_item):
        self.add_patch(junction_item)
        if junction_item in self.junction_indexes:
            return self.clean_number(self.junction_indexes[junction_item])
        return self.clean_number(junction_item)

    def get_character_index(self, character):
        names = [n.lower() for n in self.character_names]
        if character.lower() in names:
            return names.index(character.lower())
        return self.clean_number(character)

    def get_esper_index(self, esper):
        names = [n.lower() for n in self.esper_names]
        if esper.lower() in names:
            return names.index(esper.lower())
        return self.clean_number(esper)

    def get_equip_index(self, equip):
        names = [n.lower() for n in self.equip_names]
        if equip.lower() in names:
            return names.index(equip.lower())
        return self.clean_number(equip)

    def populate_always(self):
        data = {0: []}
        for junction_item in self.always_whitelist:
            data[0].append(self.get_junction_index(junction_item))

        address = self.clean_number(self.always_whitelist_address)
        populate_data(data, self.outfile, address)

    def populate_list(self, category, color, max_index):
        data = {}
        import_data = getattr(self, '%s_%slist' % (category, color))
        category_cleaner = getattr(self, 'get_%s_index' % category)
        for key, junction_items in import_data.items():
            key = category_cleaner(key)
            assert key not in data
            junction_items = [self.get_junction_index(junction_item)
                              for junction_item in junction_items]
            data[key] = junction_items

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

    def write_patches(self):
        set_addressing_mode('hirom')
        for patch in sorted(self.patches):
            write_patch(self.outfile, patch)

    def execute(self):
        self.populate_everything()
        self.write_patches()


if __name__ == '__main__':
    from sys import argv
    if len(argv) > 2:
        jm = JunctionManager(argv[1], argv[2])
    else:
        jm = JunctionManager('test.smc', 'bcg_junction_manifest.json')
    jm.execute()
