from randomtools.utils import map_to_snes
from randomtools.tablereader import (
    tblpath, write_patch, set_addressing_mode, get_open_file)

import json
from os import path

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
        f.write(bytes(values))
        pointers.append(pointer)
        pointer += len(values)

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
            if isinstance(value, dict) and key.endswith('list'):
                category = key.split('_')[0]
                if category in ('character', 'esper', 'equip'):
                    category_cleaner = getattr(self, 'get_%s_index' % category)
                    new_dict = {}
                    for k, v in sorted(value.items()):
                        k = category_cleaner(k)
                        assert k not in new_dict
                        new_dict[k] = v
                    value = new_dict
                    full_data[key] = value

            if key == 'patch_list':
                new_dict = {}
                for k, v in sorted(value.items()):
                    k = self.get_junction_index(k)
                    assert k not in new_dict
                    new_dict[k] = v
                value = new_dict
                full_data[key] = value

            if isinstance(value, dict):
                if key not in ('junction_indexes', 'junction_short_names'):
                    assert all(isinstance(k, int) for k in value.keys())
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

        for category in ['character', 'esper', 'equip']:
            if force_category and category != force_category:
                continue

            category_cleaner = getattr(self, 'get_%s_index' % category)
            try:
                k = category_cleaner(key)
            except ValueError:
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

    def get_character_index(self, character):
        return self.get_category_index('character', character)

    def get_esper_index(self, esper):
        return self.get_category_index('esper', esper)

    def get_equip_index(self, equip):
        return self.get_category_index('equip', equip)

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
        category_cleaner = getattr(self, 'get_%s_index' % category)
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
