from randomtools.utils import map_to_snes, map_from_snes, md5, random
from randomtools.tablereader import (
    tblpath, write_patch, set_addressing_mode, get_open_file, verify_patchlist)

import json
from collections import defaultdict
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

def recursive_merge(original, update):
    for key in sorted(update):
        if key in original:
            assert type(original[key]) == type(update[key])
            if type(update[key]) is dict:
                original[key] = recursive_merge(original[key], update[key])
                continue
        original[key] = update[key]
    return original


class JunctionManager:
    MAX_LINE_LENGTH = 32

    def __init__(self, outfile, manifest=None, data=None, update=None):
        self.outfile = get_open_file(outfile)
        self.directory = tblpath
        self.patches = set()
        self.patch_parameters = {}

        assert manifest or data
        if manifest:
            filename = path.join(self.directory, manifest)
            with open(filename) as f:
                full_data = json.loads(f.read())
        else:
            full_data = {}

        if update is not None:
            filename = path.join(self.directory, update)
            with open(filename) as f:
                update = json.loads(f.read())
            full_data = recursive_merge(full_data, update)

        if 'address_mapping' in full_data:
            self.set_address_mapping(full_data['address_mapping'])
            del(full_data['address_mapping'])
        else:
            self.address_mapping = None

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

            if key in ('patch_list', 'junction_short_names', 'junction_tags'):
                new_dict = {}
                for k, v in sorted(value.items()):
                    k = self.get_junction_index(k)
                    assert k not in new_dict
                    new_dict[k] = v
                value = new_dict
                full_data[key] = value

            if isinstance(value, dict):
                if key not in ('junction_indexes',):
                    assert all(isinstance(k, int) for k in value.keys())

            if key.endswith('_address') or key.startswith('num_'):
                full_data[key] = self.clean_number(full_data[key])

            if key.endswith('_tags') and key != 'junction_tags':
                filename = path.join(tblpath, full_data[key])
                with open(filename) as f:
                    tags = json.loads(f.read())
                full_data[key] = tags

            setattr(self, key, full_data[key])

        for patch in self.core_patch_list:
            self.patches.add(patch)

        self.load_character_map()
        for name in self.junction_short_names.values():
            self.map_text(name)

        for key in full_data:
            if key.endswith('_tags') and key != 'junction_tags':
                tags = getattr(self, key)
                category = key.rsplit('_', 1)[0]
                tags = {self.get_category_index(category, k): v
                        for (k, v) in tags.items()}
                setattr(self, key, tags)

    @property
    def report(self):
        s = ''
        names = sorted({self.junction_short_names[i]
                        for i in self.always_whitelist})
        if names:
            s += 'ALWAYS\n  {0}\n\n'.format(', '.join(names))

        for category in ('character', 'esper', 'equip',
                         'status', 'monster'):
            if category.endswith('s'):
                substr = '{0}ES\n'.format(category.upper())
            else:
                substr = '{0}S\n'.format(category.upper())
            names = getattr(self, '%s_names' % category)
            whitelist = getattr(self, '%s_whitelist' % category)
            blacklist = getattr(self, '%s_blacklist' % category)
            any_junctions = False
            for name in sorted(names):
                category_index = self.get_category_index(category, name)
                if category_index not in whitelist:
                    whitelist[category_index] = []
                if category_index not in blacklist:
                    blacklist[category_index] = []
                junctions = whitelist[category_index]
                junctions = {self.junction_short_names[i] for i in junctions}
                nonjunctions = blacklist[category_index]
                nonjunctions = {self.junction_short_names[i]
                                for i in nonjunctions}
                junctions = {j for j in junctions if j not in nonjunctions}
                nonjunctions = {'-%s' % j for j in nonjunctions}
                junctions = sorted(junctions) + sorted(nonjunctions)
                if not junctions:
                    continue
                junctions = ', '.join(junctions)
                line = '{0:>14} : {1}\n'.format(name, junctions)
                substr += line
                any_junctions = True
            if any_junctions:
                s += substr + '\n'
        return s.rstrip()

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

    def enable_all_patches(self):
        for junction_index in self.patch_list:
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

    def reverse_index(self, category, index):
        names = getattr(self, '%s_names' % category)
        name = names[index]
        return name

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
            write_patch(self.outfile, patch, mapping=self.address_mapping,
                        parameters=self.patch_parameters)

    def load_character_map(self):
        self.character_map = {}
        with open(path.join(tblpath, self.character_table)) as f:
            for line in f:
                character, code = line.split()
                code = int(code, 0x10)
                assert code not in self.character_map
                self.character_map[code] = character
                if character not in self.character_map:
                    self.character_map[character] = code

        for (a, b) in {
                ' ': 0xff,
                '\n': 0x01,
                }.items():
            self.character_map[a] = b
            self.character_map[b] = a

    def map_character(self, c):
        if isinstance(c, bytes):
            c = int.from_bytes(c, byteorder='big')

        try:
            c = self.character_map[c]
        except KeyError:
            try:
                c = '<{0:0>2x}>'.format(c)
            except:
                import pdb; pdb.set_trace()

        if c == '\n':
            c = '<01>'
        if isinstance(c, int):
            if c >= 0x100:
                c = c.to_bytes(length=2, byteorder='big')
            else:
                c = c.to_bytes(length=1, byteorder='big')

        return c

    def map_text(self, text):
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
            elif isinstance(text, str):
                c = self.map_character(text[0])
                text = text[1:]
                result += c
            elif isinstance(text, bytes):
                test = text[:2]
                if test.startswith(b'\x00'):
                    test = text[:1]
                c = self.map_character(test)
                if isinstance(c, str) and c.startswith('<') and len(test) > 1:
                    c = self.map_character(text[0])
                    text = text[1:]
                else:
                    text = text[len(test):]
                result += c
        return result

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
            message = self.map_text(message)
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
            m = self.map_text(message) + b'\x00'
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
            if 'J' in self.character_map:
                s = 'J:'
            else:
                s = ''
            for junction_index in values:
                if ',' in self.character_map:
                    append = ' %s,' % self.junction_short_names[junction_index]
                else:
                    append = '%s、' % self.junction_short_names[junction_index]
                test = s + append
                if len(test.split('\n')[-1]) > self.MAX_LINE_LENGTH:
                    append = '\n' + append[1:]
                s = s + append
            message = '\n'.join(s.split('\n')[:2])
            assert message[-1] in (',', '、')
            if len(message) < len(s):
                message = message[:-1] + '…'
            else:
                message = message[:-1]
            if ',' not in self.character_map:
                assert ',' not in message
            messages[key] = message
        return messages

    def rewrite_descriptions(self):
        messages = self.generate_descriptions('equip')
        self.write_descriptions(self.equip_description_address, messages,
                                self.equip_free_address)
        messages = self.generate_descriptions('esper')
        self.write_descriptions(self.esper_description_address, messages,
                                self.esper_free_address)

    def read_battle_messages(self, num_messages=256, pointers_address=None,
                             messages_address=None):
        if pointers_address is None:
            pointers_address = self.battle_message_pointer_address
        if messages_address is None:
            messages_address = self.battle_message_address
        self.outfile.seek(pointers_address)
        pointers = []
        for _ in range(num_messages):
            pointer = int.from_bytes(self.outfile.read(2), byteorder='little')
            pointers.append(pointer)
        messages = []
        for p in pointers:
            self.outfile.seek((messages_address & 0xFF8000) | p)
            message = b''
            prev_c = None
            while True:
                c = self.outfile.read(1)
                if c == b'\x00' and prev_c != b'\x12':
                    break
                message += c
                prev_c = c
            messages.append(self.map_text(message))
        return messages

    def write_battle_messages(self, messages, pointers_address=None,
                              messages_address=None):
        if pointers_address is None:
            pointers_address = self.battle_message_pointer_address
        if messages_address is None:
            messages_address = self.battle_message_address
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
                mdata = self.map_text(m)
                self.outfile.write(mdata + b'\x00')
                next_free_space += len(mdata) + 1
                if messages_address < pointers_address:
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

    def set_address_mapping(self, mapping):
        self.address_mapping = path.join(tblpath, mapping)

    def set_parameters(self, parameters):
        for k, v in parameters.items():
            self.patch_parameters[k] = v

    def set_seed(self, seed):
        self.seed = seed

    def reseed(self, salt):
        if not hasattr(self, 'seed') or self.seed is None:
            raise Exception('No seed has been set for BCG Junction.')

        self.random = random
        s = 'junction%s%s' % (self.seed, salt)
        value = int(md5(s.encode('ascii')).hexdigest(), 0x10)
        self.random.seed(value)

    def get_weights(self, junctions, all_item_tags):
        scores = {}
        weights = {}
        junction_tag_pool = {t for j in self.junction_tags
                             for t in self.junction_tags[j]}
        junction_tag_pool |= {'-%s' % t for t in junction_tag_pool}
        junction_tag_pool |= {t[1:] for t in junction_tag_pool if t[0] == '-'}
        item_tag_pool = {t for i in all_item_tags for t in all_item_tags[i]}
        item_tag_pool |= {'-%s' % t for t in item_tag_pool}
        item_tag_pool |= {t[1:] for t in item_tag_pool if t[0] == '-'}
        common_tag_pool = junction_tag_pool & item_tag_pool
        exclusive_tag_pool = ((junction_tag_pool | item_tag_pool)
                              - common_tag_pool)

        for junction_index in sorted(junctions):
            junction_tags = self.junction_tags[junction_index]
            junction_tags = [t for t in junction_tags if t in common_tag_pool]
            for item_index in sorted(all_item_tags):
                item_tags = all_item_tags[item_index]
                item_tags = [t for t in item_tags if t in common_tag_pool]
                score = 1
                for j in junction_tags:
                    for i in item_tags:
                        if j == i:
                            score += 1
                        elif j == '-%s' % i or i == '-%s' % j:
                            score -= 1
                score = score / ((len(junction_tags) * len(item_tags)) + 2)
                scores[(item_index, junction_index)] = (
                    score, random.random(), item_index, junction_index)

        sorted_item_junctions = {}
        for item_index in sorted(all_item_tags):
            sorted_junctions = sorted(
                junctions, key=lambda j: scores[(item_index, j)])
            sorted_item_junctions[item_index] = sorted_junctions
        for junction_index in sorted(junctions):
            sorted_items = sorted(all_item_tags,
                                  key=lambda i: scores[(i, junction_index)])
            max_item_index = len(sorted_items)-1
            for item_index in sorted(all_item_tags):
                sorted_junctions = sorted_item_junctions[item_index]
                max_junction_index = len(sorted_junctions)-1
                item_rank = sorted_items.index(item_index) / max_item_index
                junction_rank = (sorted_junctions.index(junction_index)
                                 / max_junction_index)
                weight = item_rank * junction_rank
                if item_index not in weights:
                    weights[item_index] = {}
                assert junction_index not in weights[item_index]
                weights[item_index][junction_index] = weight

        return weights

    def get_junction_weighted(self, item, weights):
        item_weights = weights[item]
        junction_indexes = sorted(
            item_weights,
            key=lambda j: (item_weights[j], random.random(), j))
        max_index = len(junction_indexes) - 1
        index = random.randint(random.randint(0, max_index), max_index)
        return junction_indexes[index]

        total = sum(item_weights.values())
        threshold = self.random.random() * total
        running = 0
        for junction_index in sorted(item_weights):
            running += item_weights[junction_index]
            if running >= threshold:
                return junction_index

    def randomize_sparing(self, items, category, tags=None, ratio=0.36788):
        if tags is True:
            tags = getattr(self, '%s_tags' % category)

        self.reseed(category)
        options = [0 for _ in items]
        max_index = len(options) - 1
        for _ in range(int(round(len(items) * ratio))):
            options[self.random.randint(0, max_index)] += 1
        options = [min(o, 3) for o in options]

        banlist = getattr(self, '%s_banlist' % category)
        banlist += self.always_banlist
        banlist = {self.get_junction_index(i) for i in banlist}
        junctions = sorted({self.get_junction_index(i)
                            for i in self.junction_indexes} - banlist)
        assert not set(junctions) & banlist

        if tags is not None:
            tags = {self.get_category_index(category, i): ts
                    for (i, ts) in tags.items()}
            weights = self.get_weights(junctions, tags)

        for item in sorted(items):
            option = self.random.choice(options)
            for _ in range(option):
                if tags is None:
                    junction = self.random.choice(junctions)
                else:
                    junction = self.get_junction_weighted(item, weights)
                self.add_junction(item, junction, force_category=category)

    def randomize_generous(self, items, category, tags=None):
        if tags is True:
            tags = getattr(self, '%s_tags' % category)

        self.reseed(category)
        banlist = getattr(self, '%s_banlist' % category)
        banlist += self.always_banlist
        banlist = {self.get_junction_index(i) for i in banlist}
        junctions = sorted({self.get_junction_index(i)
                            for i in self.junction_indexes} - banlist)
        if tags is not None:
            tags = {self.get_category_index(category, i): ts
                    for (i, ts) in tags.items()}
            weights = self.get_weights(junctions, tags)

        for item in sorted(items):
            option = 1 + random.randint(0, 1) + random.randint(0, 1)
            for _ in range(option):
                if tags is None:
                    junction = self.random.choice(junctions)
                else:
                    junction = self.get_junction_weighted(item, weights)
                self.add_junction(item, junction, force_category=category)

    def match_esper_monster_junctions(self, matches=None):
        if matches is None:
            matches = defaultdict(set)
            for esper_name in self.esper_names:
                key = ('%s ' % esper_name).lower()
                for monster_name in self.monster_names:
                    if monster_name.lower().startswith(key):
                        matches[esper_name].add(monster_name)

        for esper_index, monster_indexes in sorted(matches.items()):
            esper_index = self.get_category_index('esper', esper_index)
            esper_junctions = self.esper_whitelist[esper_index]
            for monster_index in monster_indexes:
                monster_index = self.get_category_index('monster',
                                                        monster_index)
                self.monster_whitelist[monster_index] = esper_junctions

    def relocate_command_pointers(self):
        num_commands = 0x34
        self.outfile.seek(self.command_ptrs_address)
        pointers = self.outfile.read(num_commands * 2)
        self.outfile.seek(self.command_ptrs_relocated_address)
        self.outfile.write(pointers)

    def execute(self):
        self.populate_everything()
        self.enable_all_patches()
        self.write_patches()
        self.relocate_command_pointers()
        self.rewrite_descriptions()
        self.rewrite_battle_messages()

    def verify_patches(self):
        verify_patchlist(self.outfile, sorted(self.patches))

    def verify_command_pointers(self):
        num_commands = 0x34
        self.outfile.seek(self.command_ptrs_address)
        a = self.outfile.read(num_commands * 2)
        self.outfile.seek(self.command_ptrs_relocated_address)
        b = self.outfile.read(num_commands * 2)
        assert a == b

    def verify(self):
        self.outfile = get_open_file(self.outfile)
        self.verify_patches()
        self.verify_command_pointers()


if __name__ == '__main__':
    from sys import argv
    if len(argv) > 2:
        jm = JunctionManager(argv[1], argv[2])
    else:
        jm = JunctionManager('test.smc', 'bcg_junction_manifest.json')
    jm.execute()
