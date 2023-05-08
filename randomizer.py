from randomtools.tablereader import (
    TableObject, get_global_label, addresses, names, gen_random_normal,
    get_activated_patches, mutate_normal, shuffle_normal, write_patch,
    get_random_degree, tblpath, get_open_file)
from randomtools.utils import (
    classproperty, cached_property, utilrandom as random)
from randomtools.interface import (
    get_outfile, get_seed, get_flags, get_activated_codes, activate_code,
    run_interface, rewrite_snes_meta, clean_and_write, finish_interface)

from bcg_junction import JunctionManager
from ex_utils import generate_character_palette, shuffle_char_hues
from collections import Counter, defaultdict
from time import time, gmtime
from itertools import combinations
from os import path
from traceback import format_exc


VERSION = "6.2"
ALL_OBJECTS = None
DEBUG_MODE = True
FOOLS = False

price_message_indexes = {
    10:     0xa6b,
    50:     0xa6c,
    100:    0xa6d,
    500:    0xa6e,
    1000:   0xa6f,
    1500:   0xa70,
    2000:   0xa71,
    3000:   0xa72,
    4000:   0xa73,
    5000:   0xa74,
    7000:   0xa75,
    8000:   0xa5f,
    10000:  0xa63,
    12000:  0xa60,
    15000:  0xa61,
    18000:  0xa62,
    20000:  0xa67,
    30000:  0xa64,
    40000:  0xa65,
    50000:  0xa68,
    #60000:  0xa66,
    }


def to_ascii(text):
    s = ''
    for c in text:
        if 0x80 <= c <= 0x99:
            s += chr(ord('A') + c-0x80)
        elif 0x9A <= c <= 0xB3:
            s += chr(ord('a') + c-0x9A)
        elif 0xB4 <= c <= 0xBD:
            s += chr(ord('0') + c-0xB4)
        else:
            s += '_'
    return s


def bnw_encode(text):
    table = {
        0x80: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
        0x9a: 'abcdefghijklmnopqrstuvwxyz',
        0xb4: '0123456789!?',
        0xc0: '/:"\'-.,…;#+()%~✓',
        0xff: ' ',
        }
    for row_index, chars in sorted(table.items()):
        for i, c in enumerate(chars):
            index = row_index + i
            assert c not in table
            assert index not in table.values()
            table[c] = index
    text = [table[c] for c in text]
    return bytes(text)


def int_to_bytelist(value, length):
    value_list = []
    for _ in range(length):
        value_list.append(value & 0xFF)
        value >>= 8
    assert value == 0
    return value_list


class PaletteMixin(TableObject):
    @classmethod
    def color_to_rgb(cls, color):
        r = color & 0b11111
        g = (color >> 5) & 0b11111
        b = (color >> 10) & 0b11111
        return r, g, b

    @classmethod
    def rgb_to_hsl(cls, r, g, b):
        r = r / float(0b11111)
        g = g / float(0b11111)
        b = b / float(0b11111)

        minval = min(r, g, b)
        maxval = max(r, g, b)
        luminance = (maxval + minval) / 2
        if maxval == minval:
            saturation = 0
        elif luminance <= 0.5:
            saturation = (maxval-minval) / (maxval + minval)
        else:
            saturation = (maxval-minval) / (2.0-(maxval + minval))

        saturation = round(saturation, 10)
        assert 0 <= saturation <= 1
        luminance = round(luminance, 10)
        assert 0 <= luminance <= 1

        if maxval == minval:
            hue = 0
        elif r == maxval:
            hue = (g-b)/(maxval-minval)
        elif g == maxval:
            hue = 2.0 + ((b-r)/(maxval-minval))
        elif b == maxval:
            hue = 4.0 + ((r-g)/(maxval-minval))
        hue *= 60

        r = int(round(r * 0b11111))
        g = int(round(g * 0b11111))
        b = int(round(b * 0b11111))

        #assert (r, g, b) == cls.hsl_to_rgb(hue, saturation, luminance)
        return hue, saturation, luminance

    @classmethod
    def hsl_to_rgb(cls, hue, saturation, luminance):
        if saturation <= 0:
            r, g, b = luminance, luminance, luminance
            r = int(round(r*0b11111))
            g = int(round(g*0b11111))
            b = int(round(b*0b11111))
            return r, g, b

        if luminance < 0.5:
            t1 = luminance * (1.0+saturation)
        else:
            t1 = luminance + saturation - (luminance*saturation)
        t2 = (2*luminance) - t1
        hue /= 360.0
        colordict = {}
        tempdict = {}
        tempdict['r'] = (hue + 0.333) % 1
        tempdict['g'] = hue
        tempdict['b'] = (hue - 0.333) % 1

        for key, value in tempdict.items():
            if (6*value) < 1:
                newval = t2 + ((t1-t2) * 6 * value)
            elif (2*value) < 1:
                newval = t1
            elif (3*value) < 2:
                newval = t2 + ((t1-t2) * ((2/3.0)-value) * 6)
            else:
                newval = t2
            colordict[key] = newval

        r = int(round(colordict['r']*0b11111))
        g = int(round(colordict['g']*0b11111))
        b = int(round(colordict['b']*0b11111))
        return r, g, b

    @classmethod
    def calculate_hue_distance(self, h1, h2):
        h1, h2 = sorted((h1, h2))
        return min(abs(h1-h2), abs((h1+360)-h2))

    def get_color_hsl(self, color_index):
        color = self.colors[color_index]
        r, g, b = self.color_to_rgb(color)
        h, s, l = self.rgb_to_hsl(r, g, b)
        return h, s, l

    def set_color_hsl(self, color_index, h, s, l):
        r, g, b = self.hsl_to_rgb(h, s, l)
        self.set_color(color_index, r, g, b)

    def set_color(self, color_index, r, g, b):
        val = r
        val |= (g << 5)
        val |= (b << 10)
        self.colors[color_index] = val

    def calculate_clusters(self):
        NUM_TESTS = 8
        if hasattr(self, 'valid_indexes'):
            hsls = [self.get_color_hsl(i) for i in self.valid_indexes]
        else:
            hsls = [self.get_color_hsl(i) for i in range(len(self.colors))]
        clusters_candidates = []
        for _ in range(NUM_TESTS):
            starters = random.sample(list(range(len(hsls))),
                                     random.randint(2, 3))
            enumerated_hsls = list(enumerate(hsls))
            random.shuffle(enumerated_hsls)
            clusters = [{i: hsls[i]} for i in sorted(starters)]
            for i, (h, s, l) in enumerated_hsls:
                if s > 0:
                    scores = {}
                    for cluster in clusters:
                        hues = [ch for (ch, cs, cl) in cluster.values()]
                        score = sum([(self.calculate_hue_distance(ch, h)**2)
                                     for ch in hues]) / len(hues)
                        scores[clusters.index(cluster)] = score
                    best_cluster = min(scores, key=lambda c: scores[c])
                    best_cluster = clusters[best_cluster]
                else:
                    best_cluster = random.choice(clusters)
                if i in best_cluster:
                    assert best_cluster[i] == (h, s, l)
                else:
                    best_cluster[i] = (h, s, l)
            clusters = [set(c.keys()) for c in clusters]
            running_total = set()
            for c in clusters:
                if c & running_total:
                    clusters = [{color for c in clusters for color in c}]
                    break
                running_total |= c
            clusters_candidates.append(clusters)

        scores = {}
        for i, clusters in enumerate(clusters_candidates):
            if len(clusters) == 1:
                length_score = 0
            else:
                lengths = [len(c) for c in clusters]
                num_values = sum(lengths)
                max_difference = num_values + 1 - len(clusters)
                if num_values / len(clusters) == num_values // len(clusters):
                    min_difference = 0
                else:
                    min_difference = 1
                minlen, maxlen = min(lengths), max(lengths)
                length_score = (((maxlen - minlen) - min_difference)
                                / (max_difference - min_difference))
                length_score = 1 - length_score
                if minlen == maxlen:
                    assert length_score == 1
                if maxlen - minlen == max_difference:
                    assert length_score == 0
            assert 0 <= length_score <= 1

            grouping_score = []
            for c in clusters:
                hsls = [self.get_color_hsl(i) for i in c]
                hues = [h for (h, s, l) in hsls if s > 0]
                if len(hues) == 0:
                    continue
                avg = sum(hues) / len(hues)
                distances = [self.calculate_hue_distance(avg, h)
                             for h in hues]
                avg = sum(distances) / len(distances)
                assert 0 <= avg / 180 <= 1
                grouping_score.append(avg / 180)
            if len(grouping_score) == 0:
                score = length_score
            else:
                grouping_score = sum(grouping_score) / len(grouping_score)
                score = (length_score + (grouping_score*2)) / 3
            scores[i] = (score, i)

        index = max(scores, key=lambda i: scores[i])
        return clusters_candidates[index]

    def recolor_by_cluster(self, clusters=None, hue_randomization='select',
                           hue_offset=None):
        if hue_offset is not None:
            assert hue_randomization == 'offset'
        if clusters == 'all':
            clusters = [set(range(len(self.colors)))]
        if not hasattr(self.__class__, '_hue_options'):
            self.__class__._hue_options = []
            for o in self.every:
                for (i, color) in enumerate(self.colors):
                    h, s, l = self.get_color_hsl(i)
                    if s > 0:
                        self.__class__._hue_options.append(h)
            self.__class__._hue_options = sorted(self.__class__._hue_options)

        if clusters is None:
            clusters = self.calculate_clusters()

        done_hues = []
        for cluster in clusters:
            saturated = [i for i in range(len(cluster))
                         if self.get_color_hsl(i)[1] > 0]
            if not saturated:
                continue
            old_h, old_s, old_l = self.get_color_hsl(
                random.choice(saturated))
            assert old_s > 0

            for _ in range(100):
                if hue_randomization == 'random':
                    target_hue = random.random() * 360
                elif hue_randomization == 'select':
                    target_hue = random.choice(self._hue_options)
                elif hue_randomization == 'offset':
                    assert hue_offset is not None
                    break
                else:
                    target_hue = hue_randomization
                    break

                finished = False
                for done_hue in sorted(done_hues) + [old_h]:
                    diff = self.calculate_hue_distance(done_hue,
                                                       target_hue)
                    diff = diff / 180.0
                    assert 0 <= diff <= 1
                    if random.random() < diff:
                        finished = True
                if finished:
                    break

            if hue_randomization != 'offset':
                done_hues.append(target_hue)
                hue_offset = None

                options = [i for i in sorted(cluster)
                           if self.get_color_hsl(i)[1] > 0]
                if not options:
                    options = sorted(cluster)

                chosen = random.choice(options)
                h, s, l = self.get_color_hsl(chosen)
                hue_offset = (target_hue - h) % 360
                assert (round((h + hue_offset) % 360, 10)
                        == round(target_hue % 360, 10))

            saturation_factor = mutate_normal(0.5, 0, 1, random_degree=0.5,
                                              return_float=True)
            saturation_factor = (saturation_factor * 2) - 1
            for index in cluster:
                h, s, l = self.get_color_hsl(index)
                h = (h + hue_offset) % 360
                old_s = s
                if saturation_factor > 0:
                    s = s ** (1-saturation_factor)
                    assert s >= old_s
                else:
                    s = 1 - s
                    s = s ** (1-abs(saturation_factor))
                    s = 1 - s
                    assert s <= old_s
                self.set_color_hsl(index, h, s, l)


class VanillaObject(TableObject):
    flag = 'v'
    flag_description = 'nothing'


class MusicObject(TableObject):
    flag = 'u'
    flag_description = 'music'

    def randomize(self):
        from johnnydmad.johnnydmad import (
            BASEPATH,
            process_music, process_formation_music_by_table, process_map_music,
            add_music_player, get_music_spoiler, random as music_random)

        music_random.seed(get_seed())

        if ('SAFE_MODE' not in get_global_label()
                and 'BNW' not in get_global_label()
                and 'JP' not in get_global_label()):
            write_patch(get_outfile(), 'patch_music_player.txt')

        with open(get_outfile(), 'rb') as f:
            outrom = f.read()

        subpath = path.join(BASEPATH, 'johnnydmad')
        metadata = {}
        playlist_filename = path.join(tblpath, 'playlist.txt')
        freespace = ['410000-57FFFF']
        outrom = process_music(outrom, playlist_filename=playlist_filename,
                               subpath=subpath, meta=metadata,
                               freespace=freespace)

        if 'BNW' in get_global_label():
            outrom = process_formation_music_by_table(
                outrom, form_music=path.join(tblpath,
                                             'formationmusic_bnw.txt'))
        else:
            outrom = process_formation_music_by_table(
                outrom, form_music=path.join(tblpath, 'formationmusic.txt'))
        outrom = process_map_music(outrom, conditional_narshe_mines=False)
        if 'patch_music_player.txt' in get_activated_patches():
            outrom = add_music_player(outrom, metadata=metadata)

        with open(get_outfile(), 'wb') as f:
            f.write(outrom)

        f = get_open_file(get_outfile())
        f.flush()
        for fmo in FormationMetaObject.every:
            fmo.reload_randomized_music()
        for l in LocationObject.every:
            l.reload_randomized_music()

        spoiler = get_music_spoiler()
        spoiler_filename = 'music.{0}.txt'.format(get_seed())
        with open(spoiler_filename, 'w+') as f:
            f.write(spoiler)


class OverworldRateObject(TableObject): pass
class DungeonRateObject(TableObject): pass
class InitialMembitObject(TableObject):
    @classmethod
    def set_membit(self, address, bit=None, value=True):
        assert isinstance(value, bool)
        if bit is None:
            address, bit = (address >> 3), address & 7
        else:
            assert not isinstance(bit, bool)
        InitialMembitObject.get(address & 0x7F).set_own_bit(bit, value)

    def set_own_bit(self, bit, value):
        mask = (1 << bit)
        if value:
            self.membyte |= mask
        else:
            self.membyte = (self.membyte | mask) ^ mask


class RNGObject(TableObject):
    flag = 'r'
    flag_description = 'RNG'

    intershuffle_attributes = ['value']


class CmdMenuPtrObject(TableObject): pass
class MagitekTargetObject(TableObject): pass
class MagitekSkillObject(TableObject): pass
class CmdPtrObject(TableObject): pass
class SlotsObject(TableObject): pass


class CmdChangeMixin(object):
    @property
    def equipment_bit(self):
        i = (4-self.index) + 2
        return (1 << i)


class CmdChangeFAObject(TableObject, CmdChangeMixin):
    @classproperty
    def after_order(cls):
        return [CmdChangeFBObject]

    def cleanup(self):
        self.command = CmdChangeFBObject.get(self.index).command


class CmdChangeTAObject(TableObject, CmdChangeMixin):
    @classproperty
    def after_order(cls):
        return [CmdChangeTBObject]

    def cleanup(self):
        self.command = CmdChangeTBObject.get(self.index).command


class CharPaletteObject(TableObject):
    flag = 'l'

    @classmethod
    def randomize_all(cls):
        super().randomize_all()

        unique = {0, 1, 4}
        unique_used = set()
        candidates = list(range(6)) * 3
        random.shuffle(candidates)
        for cpo, p in zip(CharPaletteObject.every, candidates):
            cpo.palette_index = p
            if cpo.index in unique:
                if p in unique_used:
                    fallback = [i for i in range(6) if i not in unique_used]
                    cpo.palette_index = random.choice(fallback)
                unique_used.add(cpo.palette_index)

    def preclean(self):
        related = {5: 4}
        if self.index in related:
            relative = related[self.index]
            self.palette_index = CharPaletteObject.get(relative).palette_index


class MouldObject(TableObject): pass

class CmdChangeFBObject(TableObject, CmdChangeMixin):
    flag = 'o'
    flag_description = 'character commands'

    def __repr__(self):
        return '{0:0>2X} -> {1:0>2X}'.format(
            self.command, CmdChangeTBObject.get(self.index).command)

    @classmethod
    def randomize_all(cls):
        cls.class_reseed('rand_commands')
        for c in CharacterObject.every:
            assert not hasattr(c, 'randomized')
            c.randomize_commands()
        super().randomize_all()

    def randomize(self):
        valid_commands = sorted(CharacterObject.current_initial_commands)
        self.command = random.choice(valid_commands)


class CmdChangeTBObject(TableObject, CmdChangeMixin):
    @classproperty
    def after_order(cls):
        return [CharacterObject, CmdChangeFBObject]

    def randomize(self):
        from_command = CmdChangeFBObject.get(self.index).command
        existing_commands = sorted(CharacterObject.current_initial_commands)
        potential_commands = [c for c in CharacterObject.valid_commands
                              if c not in existing_commands]
        if from_command in potential_commands:
            potential_commands.remove(from_command)
        seen_commands = [tb.command for tb in CmdChangeTBObject.every
                         if hasattr(tb, 'randomized') and tb.randomized]
        temp = [c for c in potential_commands if c not in seen_commands]
        if temp:
            potential_commands = temp
        companion = CmdChangeFBObject.get(self.index)
        if self.command in existing_commands and (
                potential_commands or companion.command == self.command
                or random.choice([True, False])):
            if not potential_commands:
                banned_commands = {self.companion.command}
                for c in CharacterObject.every[:14]:
                    if self.companion.command in c.commands:
                        banned_commands |= set(c.commands)
                for c in CharacterObject.every[:14]:
                    if (self.companion.command in c.commands
                            and 0x02 not in c.commands
                            and 0x17 not in c.commands):
                        banned_commands -= {0x02, 0x17}
                potential_commands = [c for c in existing_commands
                                      if c not in banned_commands]
                if not potential_commands:
                    potential_commands = existing_commands

            self.command = random.choice(potential_commands)

    @classmethod
    def randomize_all(cls):
        CmdChangeTBObject.class_reseed('ran_order')
        objs = list(CmdChangeTBObject.every)
        random.shuffle(objs)
        for o in objs:
            if hasattr(o, 'randomized') and o.randomized:
                continue
            o.reseed(salt='ran')
            o.randomize()
            o.randomized = True


class PortraitPalObject(TableObject): pass
class PortraitPtrObject(TableObject): pass
class EventObject(TableObject): pass

class NpcObject(TableObject):
    done_pay_saves = {}

    @classproperty
    def after_order(self):
        return [CharPaletteObject]

    @property
    def event_addr(self):
        return self.misc & 0x3FFFF

    @property
    def palette(self):
        return (self.misc >> 18) & 7

    @property
    def membit(self):
        return self.misc >> 22

    def set_event_addr(self, event_addr):
        self.misc |= 0x3FFFF
        self.misc ^= 0x3FFFF
        self.misc |= event_addr

    def set_palette(self, palette_index):
        mask = 0x7 << 18
        self.misc |= mask
        self.misc ^= mask
        self.misc |= (palette_index << 18)
        assert self.palette == palette_index

    def set_membit(self, membit):
        mask = 0x3FF << 22
        self.misc |= mask
        self.misc ^= mask
        self.misc |= (membit << 22)
        assert self.membit == membit

    def become_pay_save(self, pointer, price, price_message, pay_save_command,
                        write_event):
        self.graphics = 0x6F
        self.set_palette(6)
        self.facing = 0x43
        if price in self.done_pay_saves:
            self.set_event_addr(self.done_pay_saves[price])
            return

        yes_p = pointer + 13
        no_p = yes_p + 7
        script = [
            0x4B] + int_to_bytelist(price_message, 2) + [   # show price
            0x4B] + int_to_bytelist(addresses.ask_message | 0x8000, 2) + [
            0xB6] + int_to_bytelist(yes_p, 3) + int_to_bytelist(no_p, 3) + [
            0x85] + int_to_bytelist(price, 2)               # take money
        script += pay_save_command + [0xFE]
        assert script[no_p-pointer:] == [0xFE]
        event_addr = write_event(script) - 0xA0000
        self.set_event_addr(event_addr)
        self.done_pay_saves[price] = event_addr

    def cleanup(self):
        if self.palette >= 0x6:
            return

        alternate_costumes = {
            0x41: 0x06,
            }

        if self.graphics in alternate_costumes:
            cpo = CharPaletteObject.get(alternate_costumes[self.graphics])
            assert self.palette == cpo.old_data['palette_index']
            self.set_palette(cpo.palette_index)

        if self.graphics <= 0xd:
            cpo = CharPaletteObject.get(self.graphics)
            if self.palette == cpo.old_data['palette_index']:
                self.set_palette(cpo.palette_index)
            else:
                new_palettes = list(range(6))
                new_palettes.remove(cpo.palette_index)
                self.set_palette(random.choice(new_palettes))


class SkillObject(TableObject):
    bit_similarity_attributes = {
        'targeting': 0xff,
        'elements': 0xff,
        'misc1': 0xff,
        'misc2': 0xff,
        'misc3': 0xff,
        'statuses': 0xffffffff,
        }

    @property
    def rank(self):
        return self.mp

    @property
    def name(self):
        return names.skills[self.index]


class CharNameObject(TableObject): pass
class BlitzInputObject(TableObject): pass

class InitialRageObject(TableObject):
    def cleanup(self):
        if self.index == 0 and not any(iro.initial_rages
                                       for iro in InitialRageObject.every):
            self.initial_rages |= 1


class ShopObject(TableObject):
    flag = 'p'
    flag_description = 'shops'
    #custom_random_enable = True

    def __repr__(self):
        s = '%s SHOP %x\n' % (self.shop_type.upper(), self.index)
        for i in self.items:
            s += '%s %s\n' % (str(i), i.price)
        return s.strip()

    @property
    def items(self):
        return [ItemObject.get(i) for i in self.item_ids if i < 0xFF]

    @property
    def old_items(self):
        return [ItemObject.get(i) for i in self.old_data['item_ids']
                if i < 0xFF]

    @property
    def shop_type(self):
        shop_types = {1:'weapons', 2:'armor', 3:'items', 4:'relics', 5:'misc'}
        return shop_types[self.misc & 0x7]

    @property
    def rank(self):
        if len(self.old_items) <= 0:
            return -1
        if set(self.old_data['item_ids']) == {255}:
            return -1
        return max(i.price for i in self.old_items)

    @property
    def current_rank(self):
        if len(self.items) <= 0:
            return -1
        if set(self.item_ids) == {255}:
            return -1
        return sum(i.price for i in self.items) / float(len(self.items))

    @classproperty
    def consumables(cls):
        if hasattr(ShopObject, '_consumables'):
            return ShopObject._consumables
        consumables = set([])
        for s in ShopObject.every:
            if s.shop_type == 'items':
                consumables |= set(s.old_items)
        ShopObject._consumables = sorted(consumables, key=lambda i: i.index)
        return ShopObject.consumables

    def mutate(self):
        items = list(self.items)
        num_items = len(items)
        if num_items <= 0:
            return
        candidate_shops = [
            s for s in ShopObject.every if s.shop_type == self.shop_type
            or (self.shop_type == 'misc' and s.shop_type == 'items')]
        candidate_shops = [c for c in candidate_shops
                           if c.rank >= 0 and len(c.old_items) > 0]
        num_items = len(random.choice(candidate_shops).old_items)
        num_items = mutate_normal(num_items, 1, 8, wide=False,
                                  random_degree=self.random_degree)

        candidates = []
        for s in candidate_shops:
            candidates.extend(s.old_data['item_ids'])
        candidates = [c for c in candidates if c != 0xff]

        new_items = []
        while len(new_items) < num_items:
            remaining = num_items - len(new_items)
            new_items += random.sample(candidates, remaining)
            new_items = sorted(set(new_items))
            candidates = [c for c in candidates if c not in new_items]

        new_items = sorted(set(new_items))

        if self.index == 0xc and 0xfe in self.old_data['item_ids']:
            # dried meat in mobliz
            if len(new_items) == 8:
                new_items.remove(random.choice(new_items))
            max_index = len(new_items)-1
            new_items.insert(random.randint(0, max_index), 0xfe)

        while len(new_items) < 8:
            new_items.append(0xff)

        assert len(new_items) == 8
        self.item_ids = new_items

    def cleanup(self):
        self.reseed('cleanup')
        if 'fanatix' in get_activated_codes() and self.shop_type == 'items':
            for banned_id in [0xF6, 0xF7]:
                if banned_id not in self.item_ids:
                    continue
                consumables = [i for i in self.consumables
                               if i.index not in self.item_ids]
                i = random.choice(consumables)
                self.item_ids.remove(banned_id)
                self.item_ids.append(i.index)
        self.item_ids = sorted(self.item_ids)


class MetamorphObject(TableObject):
    def __repr__(self):
        s = 'MORPH %x\n' % self.index
        for i in self.items:
            s += str(i.name) + '\n'
        return s.strip()

    @property
    def items(self):
        return [ItemObject.get(i) for i in self.item_ids if i <= 0xFE]


class MagiciteObject(TableObject):
    flag = 'g'
    flag_description = 'magicite'

    @classmethod
    def randomize_all(cls):
        MagiciteObject.class_reseed('ran')
        indexes = sorted(set([m.esper_index for m in MagiciteObject.every
                              if m.instruction in [0x86, 0x87]]))
        shuffled = list(indexes)
        random.shuffle(shuffled)
        for m in MagiciteObject.every:
            if m.esper_index not in indexes:
                continue
            index = indexes.index(m.esper_index)
            m.esper_index = shuffled[index]
            m.randomized = True

    def cleanup(self):
        try:
            assert self.instruction in [0x86, 0x87]
            assert 0x36 <= self.esper_index <= 0x50
        except AssertionError:
            if 'SAFE_MODE' in get_global_label():
                self.assert_unchanged()
            else:
                raise AssertionError


class EventSpriteObject(TableObject):
    def cleanup(self):
        try:
            assert self.thirty_seven == 0x37
            assert self.forty_three == 0x43
            assert self.actor1 == self.actor2
            assert self.old_data['actor1'] == self.old_data['actor2']

            if self.sprite <= 0xd:
                cpo = CharPaletteObject.get(self.sprite)
                assert (self.old_data['palette'] ==
                        cpo.old_data['palette_index'])
                self.palette = cpo.palette_index
        except AssertionError:
            if 'SAFE_MODE' in get_global_label():
                self.assert_unchanged()
            else:
                raise Exception('Something wrong with EventSpriteObject.')


class DialoguePtrObject(TableObject):
    @classmethod
    def bring_back_auction_prices(cls):
        if 'BNW' not in get_global_label():
            raise NotImplementedError

        indexes = sorted(price_message_indexes.values())
        assert all([i & 0xa00 == 0xa00 for i in indexes])
        pointer = min([DialoguePtrObject.get(i).dialogue_pointer
                       for i in indexes]) | 0xE0000
        message_head = b'\x01\x14\x08'
        message_tail = b'\x7f\x26\x2f\x5e\x00'
        reverse_dict = dict([(v, k) for (k, v)
                             in price_message_indexes.items()])
        f = open(get_outfile(), 'r+b')
        for i in indexes:
            dpo = DialoguePtrObject.get(i)
            dpo.dialogue_pointer = pointer & 0xFFFF
            value = str(reverse_dict[i])
            content = b''
            for c in value:
                content += bytes([0x54 + int(c)])
            f.seek(pointer)
            s = message_head + content + message_tail
            f.write(s)
            pointer += len(s)


class MonsterObject(TableObject):
    flag = 'm'
    flag_description = 'monsters'
    custom_random_enable = True
    custom_difficulty_enable = False

    magic_mutate_bit_attributes = {
        ('statuses', 'immunities'): (0xFFFFFFFF, 0xFFFFFF),
        ('absorb', 'null', 'weakness'): (0xFF, 0xFF, 0xFF),
        }

    mutate_attributes = {
        'speed': None,
        'attack': None,
        'hit': None,
        'evade': None,
        'mblock': None,
        'def': None,
        'mdef': None,
        'mpow': None,
        'hp': None,
        'mp': None,
        'xp': None,
        'gp': None,
        'level': None,
        }

    randomselect_attributes = [
        'speed', 'attack', 'hit', 'evade', 'mblock', 'def', 'mdef', 'mpow',
        'hp', 'xp', 'gp', 'level', 'morph_id', 'animation', 'special',
        ]

    REFLECT = 0x00800000

    @classproperty
    def special_ranks(self):
        if hasattr(self, '_special_ranks'):
            return self._special_ranks
        summer_dict = defaultdict(set)
        for m in MonsterObject.every:
            special = m.old_data['special'] & 0x3f
            if special & 0x20:
                continue
            summer_dict[special].add(m.rank)
        self._special_ranks = {}
        for key in summer_dict:
            self._special_ranks[key] = (sum(summer_dict[key]) /
                                        len(summer_dict[key]))
        self._special_ranks = dict(
            [(k, n) for (n, k) in enumerate(sorted(
                self._special_ranks.keys(),
                key=lambda k2: (self._special_ranks[k2], k2)))])
        return self.special_ranks

    @classproperty
    def tutorial_banned(self):
        if 'BNW' in get_global_label():
            return ['Vargas', 'Hell_Angel', 'Templar', 'Merchant', 'Whelk']
        else:
            return []

    @classproperty
    def problematic_enemies(self):
        if 'BNW' in get_global_label():
            return ['Bomb'] + self.tutorial_banned
        else:
            return []

    @property
    def name(self):
        if 'JP' in get_global_label():
            if self.index in [0x16e, 0x16f, 0x170, 0x172, 0x173, 0x174, 0x177,
                              0x178, 0x17a, 0x17b, 0x17c, 0x17e, 0x17f]:
                return 'Event %x' % self.index
            return '%x' % self.index
        return MonsterNameObject.get(self.index).name

    @property
    def is_boss(self):
        return not self.is_farmable

    @property
    def intershuffle_valid(self):
        if 'BNW' in get_global_label():
            if self.old_data['level'] in [1, 99] or self.old_data['hp'] <= 1:
                return False
        return self.rank >= 0

    @property
    def ai(self):
        return MonsterAIObject.get(self.index)

    @property
    def hp_refills(self):
        return max(self.old_data['hp'] % 10, 1)

    @property
    def true_hp(self):
        return self.hp * self.hp_refills

    @property
    def true_hp_old(self):
        return self.old_data['hp'] * self.hp_refills

    @property
    def ai_script(self):
        return self.ai.ai_script

    @property
    def pretty_ai_script(self):
        return self.ai.pretty_ai_script

    @property
    def metamorph(self):
        return MetamorphObject.get(self.morph_id & 0x1F)

    @property
    def old_morphrate(self):
        return self.old_data['morph_id'] >> 5

    @property
    def loot(self):
        ml = MonsterLootObject.get(self.index)
        return ml.loot

    @property
    def drops(self):
        ml = MonsterLootObject.get(self.index)
        return ml.drops

    @cached_property
    def is_farmable(self):
        for f in FormationObject.every:
            if f.is_random_encounter and self in f.old_enemies:
                return True
        return False

    @property
    def rank(self):
        if hasattr(self, '_rank'):
            return self._rank

        MonsterObject.class_reseed('ranking')
        monsters = list(MonsterObject.every)
        if 'BNW' in get_global_label():
            max_hp = 65535
        else:
            max_hp = 65536

        monsters = [m for m in monsters if m.old_data['level'] > 0
                    and m.old_data['hp'] < max_hp
                    and 'Event' not in m.name and set(m.name) != {'_'}]

        score_a = lambda m: (m.old_data['level'], m.true_hp_old,
                             len(m.ai_script), m.signature)
        score_b = lambda m: (m.true_hp_old, m.old_data['level'],
                             len(m.ai_script), m.signature)
        by_a = sorted(monsters, key=score_a)
        by_b = sorted(monsters, key=score_b)

        LEVEL_MULTIPLIER = 1.5
        HP_MULTIPLIER = 1

        for m in MonsterObject.every:
            if m in monsters:
                a, b = (by_a.index(m) * LEVEL_MULTIPLIER,
                        by_b.index(m) * HP_MULTIPLIER)
                m._base_rank = max(a, b) * (a+b)
            else:
                m._base_rank = -1

        companion_dict = defaultdict(set)
        for f in FormationObject.every:
            enemies = set([m for m in f.enemies if m._base_rank >= 0])
            for m in enemies:
                companion_dict[m] |= enemies

        for m in MonsterObject.every:
            if m in companion_dict:
                companion_rank = (
                    sum([m2._base_rank for m2 in companion_dict[m]])
                    / len(companion_dict[m]))
                similarity = m._base_rank / companion_rank
                if similarity > 1:
                    similarity = 1 / similarity
                ratio = similarity / 2
                if m.is_boss:
                    m._rank = m._base_rank
                    continue
                m._rank = (m._base_rank*(1-ratio)) + (companion_rank*ratio)
            else:
                m._rank = -1

        return self.rank

    def mutate(self):
        if self.rank < 0:
            return

        if self.intershuffle_valid:
            assert self.random_selected

        super(MonsterObject, self).mutate()

        for name in self.tutorial_banned:
            if name in self.name:
                self.special = self.old_data['special']

        special = self.special & 0x3f
        if special & 0x3f in self.special_ranks:
            max_index = len(self.special_ranks)-1
            special_ratio = self.special_ranks[special] / max_index
            ranked_ratio = self.ranked_ratio or 0
            assert 0 <= special_ratio <= 1
            assert 0 <= ranked_ratio <= 1
            difference = (special_ratio - ranked_ratio) * 2
            if difference > 0:
                factor = (difference**2) * ((1-self.random_degree) ** (1/2))
                if factor >= 1 or random.random() < factor:
                    self.special = self.old_data['special']

        if not self.is_boss and self.hp > self.old_data['hp']:
            self.hp = mutate_normal(
                self.old_data['hp'], self.old_data['hp'], self.hp,
                wide=True, random_degree=self.random_degree)

        if self.level > self.old_data['level']:
            self.level = mutate_normal(
                self.old_data['level'], self.old_data['level'], self.level,
                wide=True, random_degree=self.random_degree)

        for name in self.problematic_enemies:
            if name in self.name:
                for attr in sorted(self.mutate_attributes.keys()):
                    newval = getattr(self, attr)
                    oldval = self.old_data[attr]
                    value = ((newval * self.random_degree)
                             + (oldval * (1-self.random_degree)))
                    setattr(self, attr, int(round(value)))

    def difficulty_boost(self):
        if self.random_difficulty == 1.0 or not self.ranked_ratio:
            return

        if self.random_difficulty > 1.0:
            difficulty = self.random_difficulty - 1
            difficulty = (difficulty * (self.ranked_ratio)) + 1
        else:
            difficulty = self.random_difficulty

        negative_attrs = ['speed', 'attack', 'hit', 'evade', 'mblock',
                          'def', 'mdef', 'mpow', 'hp', 'mp', 'level']
        positive_attrs = ['xp', 'gp']

        for diffattr in positive_attrs + negative_attrs:
            value = getattr(self, diffattr)
            if diffattr in negative_attrs:
                value = int(round(value * random.uniform(1.0, difficulty)))
            elif diffattr in positive_attrs:
                value = int(round(value / random.uniform(1.0, difficulty)))
            length = [l for (attr, l, _) in self.specs.attributes
                      if attr == diffattr][0]
            if length == 1:
                minval, maxval = 0, 0xFE
            elif length == 2:
                minval, maxval = 0, 0xFFFE
            else:
                minval, maxval = None, None
            maxval = max(maxval, self.old_data[diffattr])
            value = max(minval, min(maxval, value))
            setattr(self, diffattr, value)

    def preclean(self):
        self.reseed('difficulty')
        self.difficulty_boost()

    def cleanup(self):
        elements, old_elements = (self.absorb | self.null), (
            self.old_data['absorb'] | self.old_data['null'])
        if (elements & old_elements) == elements:
            self.absorb = self.old_data['absorb']
            self.null = self.old_data['null']
        if self.immunities & self.old_data['immunities'] == self.immunities:
            self.immunities = self.old_data['immunities']

        if self.is_boss and self.rank >= 0:
            for attr in sorted(self.mutate_attributes):
                setattr(self, attr, max(getattr(self, attr),
                                        self.old_data[attr]))

        self.statuses ^= (self.statuses & self.immunities)

        if 'easymodo' in get_activated_codes():
            for attr in self.mutate_attributes:
                setattr(self, attr, 1)
            self.xp = 65535
            self.gp = 65535

        if 'BNW' in get_global_label():
            self.hp = (self.hp // 10) * 10
            self.hp += self.old_data['hp'] % 10
            while self.hp >= 0xFFFE:
                self.hp -= 10

            if 'Tonberry' in self.name and self.statuses & self.REFLECT:
                self.statuses ^= self.REFLECT
                assert not self.statuses & self.REFLECT


class MonsterLootObject(TableObject):
    flag = 't'
    custom_random_enable = True

    @classproperty
    def intershuffle_attributes(self):
        if 'BNW' in get_global_label():
            return ['drop_item_ids']
        else:
            return ['steal_item_ids', 'drop_item_ids']

    @property
    def intershuffle_valid(self):
        return self.is_farmable

    @property
    def monster(self):
        return MonsterObject.get(self.index)

    @property
    def is_farmable(self):
        return self.monster.is_farmable

    @property
    def loot(self):
        return [ItemObject.get(i)
                for i in self.steal_item_ids + self.drop_item_ids if i < 0xFF]

    @property
    def drops(self):
        return [ItemObject.get(i) for i in self.drop_item_ids if i < 0xFF]

    def mutate(self):
        for i, s in enumerate(self.steal_item_ids):
            if s < 0xFF:
                s = ItemObject.get(s).get_similar(
                    random_degree=self.random_degree).index
                self.steal_item_ids[i] = s

        for i, d in enumerate(self.drop_item_ids):
            if d < 0xFF:
                d = ItemObject.get(d).get_similar(
                    random_degree=self.random_degree).index
                self.drop_item_ids[i] = d

    def cleanup(self):
        old_drops = set(self.old_data['drop_item_ids'])
        if len(old_drops) == 1 and old_drops != {0xff}:
            self.drop_item_ids = [self.drop_item_ids[0]] * 2
            assert len(set(self.drop_item_ids)) == 1

        old_steals = set(self.old_data['steal_item_ids'])
        if len(old_steals) == 1 and old_steals != {0xff}:
            self.steal_item_ids = [self.steal_item_ids[0]] * 2
            assert len(set(self.steal_item_ids)) == 1

        if 'BNW' in get_global_label():
            for n, i in enumerate(self.steal_item_ids):
                index = self.old_data['steal_item_ids'][n]
                if not 0 <= index <= 0xfe:
                    continue
                item = ItemObject.get(index)
                if 'Gum_Pod' in item.name:
                    self.steal_item_ids[n] = self.old_data['steal_item_ids'][n]


class SpecialAnimObject(TableObject): pass
class MonsterCtrlObject(TableObject): pass
class MonsterSketchObject(TableObject): pass


class MonsterRageObject(TableObject):
    @property
    def monster(self):
        return MonsterObject.get(self.index)

    @property
    def ailment_special(self):
        if self.monster.special & 0x20:
            return None
        return self.monster.special & 0x1f

    @property
    def old_ailment_special(self):
        old_special = self.monster.old_data['special']
        if old_special & 0x20:
            return None
        return old_special & 0x1f

    def cleanup(self):
        if not hasattr(MonsterRageObject, '_allowed_specials'):
            MonsterRageObject._allowed_specials = set()
            for mro in MonsterRageObject.every:
                if 0xEF in mro.old_data['commands']:
                    MonsterRageObject._allowed_specials.add(
                        mro.old_ailment_special)

        special = self.ailment_special
        if special not in MonsterRageObject._allowed_specials:
            self.commands = [0xEE if c == 0xEF else c for c in self.commands]


class PackObject(TableObject):
    def __repr__(self):
        s = '%s-PACK %x:\n%s' % (
            len(self.formations), self.index,
            '\n'.join(str(f) for f in self.formations))
        return s

    @cached_property
    def guaranteed_treasure(self):
        treasure = None
        for f in self.formations:
            if f.guaranteed_treasure is None:
                return None
            if treasure is None:
                treasure = f.guaranteed_treasure
            elif f.guaranteed_treasure.rank < treasure.rank:
                treasure = f.guaranteed_treasure
        return treasure

    @cached_property
    def treasure_rank(self):
        if self.guaranteed_treasure is None:
            return 0
        return self.guaranteed_treasure.rank

    @property
    def has_xp(self):
        return all([f.has_xp and f.rank >= 0 for f in self.formations])

    @property
    def rank(self):
        if any([f.rank < 0 for f in self.formations]):
            return -1

        if len(self.formations) == 4:
            weights = [5, 5, 5, 1]
        else:
            assert len(self.formations) == 2
            weights = [1, 1]

        rank = 0
        for w, f in zip(weights, self.formations):
            rank += (w / float(sum(weights))) * f.rank

        return rank

    @property
    def formation_ids(self):
        formation_ids = []
        for attr in ['common', 'common1', 'common2', 'common3', 'rare']:
            if hasattr(self, attr):
                formation_ids.append(getattr(self, attr))
        assert len(formation_ids) in [2, 4]
        return formation_ids

    @property
    def formations(self):
        return [FormationObject.get(fid & 0x7fff)
                for fid in self.formation_ids]

    @cached_property
    def old_formation_ids(self):
        formation_ids = []
        for attr in ['common', 'common1', 'common2', 'common3', 'rare']:
            if hasattr(self, attr):
                formation_ids.append(self.old_data[attr])
        assert len(formation_ids) in [2, 4]
        return formation_ids

    @cached_property
    def old_formations(self):
        return [FormationObject.get(fid & 0x7fff)
                for fid in self.old_formation_ids]


class FourPackObject(PackObject):
    @property
    def formations(self):
        return [FormationObject.get(self.common1 & 0x7fff),
                FormationObject.get(self.common2 & 0x7fff),
                FormationObject.get(self.common3 & 0x7fff),
                FormationObject.get(self.rare & 0x7fff),
                ]

    @cached_property
    def is_random_encounter(self):
        for l in LocationObject.every:
            if not l.get_bit('enable_encounters'):
                continue
            if self.index == l.area_pack.old_data['pack_id']:
                return True

        for zpp in ZonePackPackObject.every:
            if self.index in zpp.old_data['pack_ids']:
                return True

        return False

    def cleanup(self):
        if MusicObject.flag in get_flags():
            return
        for f in self.formations:
            f.clear_music()


class TwoPackObject(PackObject):
    @property
    def formations(self):
        return [FormationObject.get(self.common),
                FormationObject.get(self.rare),
                ]

    @property
    def is_random(self):
        return len(set(self.formations)) == 2


class ZonePackPackObject(TableObject):
    @property
    def packs(self):
        return [FourPackObject.get(pid) for pid in self.pack_ids]

    @property
    def formations(self):
        return [f for pack in self.packs for f in pack.formations]


class AreaPackObject(TableObject):
    @property
    def pack(self):
        return FourPackObject.get(self.pack_id)

    @property
    def formations(self):
        return self.pack.formations


class ZoneRateObject(TableObject):
    # first 128: 64 WoB 4x4 packs 64 WoR 4x4 packs
    # latter 104: 4 locations per zro, 2 bits per location
    def cleanup(self):
        if 'fanatix' in get_activated_codes():
            self.encounter_rates = 0


class FormationMetaObject(TableObject):
    @property
    def music(self):
        return (self.music_misc >> 3) & 0x7

    def set_music(self, music):
        self.music_misc |= (0x7 << 3)
        self.music_misc ^= (0x7 << 3)
        self.music_misc |= (music << 3)
        assert self.music == music

    def clear_music(self, force=False):
        if self.music == 0 or force:
            self.set_bit('disable_fanfare', True)
            self.set_bit('continue_current_music', True)

    def reload_randomized_music(self):
        new_data = dict(self.old_data)
        self.read_data()
        new_music = self.music
        df = self.get_bit('disable_fanfare')
        ccm = self.get_bit('continue_current_music')
        for attr, value in new_data.items():
            setattr(self, attr, value)
        self.set_music(new_music)
        self.set_bit('disable_fanfare', df)
        self.set_bit('continue_current_music', ccm)


class FormationObject(TableObject):
    custom_random_enable = 'f'

    def __repr__(self):
        s = 'FORMATION %x: %s' % (
            self.index, ' '.join(e.name for e in self.enemies))
        return s

    @cached_property
    def guaranteed_treasure(self):
        if self.rank < 0:
            return None
        guaranteed = []
        for e in self.enemies:
            drops = e.drops
            if len(drops) < 2:
                continue
            guaranteed.append(min(drops, key=lambda d: d.rank))
        if not guaranteed:
            return None
        return max(guaranteed, key=lambda d: d.rank)

    @cached_property
    def is_random_encounter(self):
        for p in FourPackObject.every:
            if (p.is_random_encounter and self in p.old_formations):
                return True
        return False

    @cached_property
    def is_rare_encounter(self):
        if not self.is_random_encounter:
            return False
        for p in FourPackObject.every:
            if self in p.formations[:3]:
                return False
        return True

    @cached_property
    def is_random_event(self):
        for p in TwoPackObject.every:
            if (p.is_random and self in p.old_formations):
                return True
        return False

    @cached_property
    def is_fixed_event(self):
        for p in TwoPackObject.every:
            if (self in p.old_formations and not p.is_random):
                return True
        return False

    @cached_property
    def is_chest_encounter(self):
        for c in ChestObject.every:
            if c.old_formation and self in c.old_formation.formations:
                return True
        return False

    @cached_property
    def is_inescapable(self):
        for e in self.enemies:
            if e.get_bit('is_inescapable'):
                return True

    @property
    def metadata(self):
        return FormationMetaObject.get(self.index)

    @property
    def has_xp(self):
        return any([e.xp > 0 and e.rank >= 0 for e in self.enemies])

    def clear_music(self, force=False):
        self.metadata.clear_music(force=force)

    @property
    def true_enemy_ids(self):
        eids = []
        for (i, eid) in enumerate(self.enemy_ids):
            if eid == 0xFF and not self.enemies_present & (1 << i):
                continue
            if self.bossbyte & (1 << i):
                eid |= 0x100
            eids.append(eid)
        return eids

    @cached_property
    def old_true_enemy_ids(self):
        eids = []
        for (i, eid) in enumerate(self.old_data['enemy_ids']):
            if eid == 0xFF and not self.old_data['enemies_present'] & (1 << i):
                continue
            if self.old_data['bossbyte'] & (1 << i):
                eid |= 0x100
            eids.append(eid)
        return eids

    @property
    def enemies(self):
        return [MonsterObject.get(eid) for eid in self.true_enemy_ids]

    @cached_property
    def old_enemies(self):
        return [MonsterObject.get(eid) for eid in self.old_true_enemy_ids]

    @property
    def rank(self):
        if hasattr(self, '_rank'):
            return self._rank

        for f in FormationObject.every:
            enemy_ranks = [e.rank for e in f.enemies if e.rank > 0]
            if not enemy_ranks:
                f._base_rank = -1
            else:
                f._base_rank = max(enemy_ranks) * (sum(enemy_ranks)**0.0625)
            f._rank = f._base_rank

        companion_dict = defaultdict(set)
        for fp in FourPackObject.every:
            formations = set([f for f in fp.formations if f._base_rank >= 0])
            for f in formations:
                companion_dict[f] |= formations

        for f in FormationObject.every:
            if f in companion_dict:
                companion_rank = (
                    sum([f2._base_rank for f2 in companion_dict[f]])
                    / len(companion_dict[f]))
                similarity = f._base_rank / companion_rank
                if similarity > 1:
                    similarity = 1 / similarity
                ratio = similarity / 2
                f._rank = (f._base_rank*(1-ratio)) + (companion_rank*ratio)

        return self.rank

    @cached_property
    def two_packs(self):
        return [p for p in TwoPackObject.every if self in p.formations]


class MonsterAIObject(TableObject):
    AICODES = {0xF0: 3, 0xF1: 1, 0xF2: 3, 0xF3: 2,
               0xF4: 3, 0xF5: 3, 0xF6: 3, 0xF7: 1,
               0xF8: 2, 0xF9: 3, 0xFA: 3, 0xFB: 2,
               0xFC: 3, 0xFD: 0, 0xFE: 0, 0xFF: 0
               }

    @cached_property
    def ai_script(self):
        pointer = addresses.ai_scripts_address + self.ai_pointer
        f = open(get_outfile(), 'r+b')
        f.seek(pointer)
        script = []
        seen = False
        while True:
            value = f.read(1)
            try:
                numargs = self.AICODES[ord(value)]
                args = f.read(numargs)
            except KeyError:
                args = b''
            script.append(value + args)
            if ord(value) == 0xFF:
                if seen:
                    break
                else:
                    seen = True
        f.close()
        return script

    @cached_property
    def hp_refills(self):
        for (i, line) in enumerate(self.ai_script):
            if line == [0xFC, 0x06, 0x36, 0x00]:
                for line2 in self.ai_script[i+1:]:
                    if line2[0] == 0xFC:
                        if line2[:2] == [0xFC, 0x0D]:
                            return line2[3] + 1
                    elif line2[0] in [0xFE, 0xFF]:
                        break
        return 1

    @cached_property
    def pretty_ai_script(self):
        s = ''
        for line in self.ai_script:
            s += ('{0:0>2X} : '.format(line[0])) + ' '.join(
                ['{0:0>2X}'.format(v) for v in line[1:]])
            s = s.strip() + '\n'
        return s.strip()


class MonsterNameObject(TableObject):
    @property
    def name(self):
        return to_ascii(self.name_text)

    @classmethod
    def full_cleanup(cls):
        if hasattr(addresses, 'sort_rages_address'):
            f = open(get_outfile(), 'r+b')
            counter = 0
            for mno in sorted(MonsterNameObject.every, key=lambda n: n.name):
                if mno.index >= 0x100:
                    continue
                f.seek(addresses.sort_rages_address + counter)
                f.write(bytes([mno.index]))
                if hasattr(addresses, 'myself_rages_address'):
                    f.seek(addresses.myself_rages_address + counter)
                    f.write(bytes([mno.index]))
                counter += 1
            assert counter <= 0x100
            f.close()

        super(MonsterNameObject, cls).full_cleanup()


class SpecialNameObject(TableObject): pass
class DanceObject(TableObject): pass


class MonsterSpriteObject(TableObject):
    @property
    def monster(self):
        return MonsterObject.get(self.index)

    @property
    def name(self):
        return self.monster.name

    @property
    def palette_index(self):
        return ((self.misc_palette_index & 0x3) << 8) | self.low_palette_index


class MonsterPaletteObject(PaletteMixin):
    flag = 'k'
    flag_description = 'monster palettes'

    @property
    def valid_indexes(self):
        if self.is_8color:
            return list(range(8))
        else:
            return list(range(16))

    @property
    def is_8color(self):
        try:
            next_pointer = self.get(self.index+1).pointer
            if next_pointer - self.pointer < 0x20:
                return True
        except KeyError:
            pass
        return False

    @cached_property
    def sprite_object(self):
        for mso in MonsterSpriteObject.every:
            index = (self.pointer - self.get(0).pointer) / 16
            if mso.palette_index == index:
                return mso

    @property
    def name(self):
        return self.sprite_object.name

    @property
    def sprite_index(self):
        return self.sprite_object.misc_sprite_pointer & 0x7FFF

    @cached_property
    def comrades(self):
        return [mpo for mpo in MonsterPaletteObject.every
                if mpo.sprite_index == self.sprite_index
                and not mpo.name.startswith('_')]

    def randomize(self):
        candidates = self.comrades
        if candidates:
            chosen = random.choice(candidates)
            self.colors = chosen.old_data['colors']
        self.recolor_by_cluster()

    def cleanup(self):
        if self.is_8color:
            self.colors[8:] = self.get(self.index+1).colors[:8]


class ItemNameObject(TableObject):
    @property
    def name(self):
        return to_ascii(self.name_text)


class FullSpriteObject(TableObject):
    def cleanup(self):
        replacement = None
        if CharPaletteObject.flag in get_flags():
            if self.index == 0xa:
                replacement = 'ogmog.bin'
            elif self.index == 0xd:
                replacement = 'ogumaro.bin'

        if replacement is not None:
            with open(path.join(tblpath, replacement), 'rb') as f:
                data = f.read()
            self.data = data


class AlmostSpriteObject(TableObject): pass

class EquipabilityObject(TableObject):
    flag = 'q'
    flag_description = 'equipment'
    custom_random_enable = 'q'

class EquipmentStatsObject(TableObject):
    flag = 'i'
    flag_description = 'equipment stats'
    custom_random_enable = 'i'

class ItemObject(TableObject):
    custom_random_enable = 'i'

    mutate_attributes = {
        'power': None,
        'price': None,
        }

    magic_mutate_bit_attributes = {
        'equipability': 0xBFFF,
        #('elements', 'elemabsorbs', 'elemnulls', 'elemweaks'): (
        #    0xFF, 0xFF, 0xFF, 0xFF),
        }

    bnw_price_correction = {
            'Bouncy_Ball': 10000,
        }

    def __repr__(self):
        return self.name

    @property
    def magic_mutate_valid(self):
        return (EquipabilityObject.flag in get_flags() and self.equipability
                and 1 <= (self.itemtype & 0x7) <= 5)

    @property
    def name(self):
        return ItemNameObject.get(self.index).name

    @property
    def is_legit(self):
        if 'BNW' in get_global_label():
            return set(self.name) != {'_'}
        return True

    @property
    def pretty_type(self):
        return {0: 'tool',
                1: 'weapon',
                2: 'armor',
                3: 'shield',
                4: 'helm',
                5: 'relic',
                6: 'consumable'}[self.itemtype & 0x7]

    @property
    def rank(self):
        if hasattr(self, '_rank'):
            return self._rank

        ItemObject.class_reseed('ranking')

        if 'BNW' in get_global_label():
            BANNED_INDEXES = [0x10]  # Narpas
        else:
            BANNED_INDEXES = []

        # TODO: also consider morphs?
        tier0 = [i for i in ItemObject.every if i.is_buyable]
        tier0 = sorted(tier0, key=lambda i: (i.is_buyable, i.signature))
        tier1 = [i for i in ItemObject.every
                 if i.is_farmable and i not in tier0]
        tier1 = sorted(tier1, key=lambda i: (i.is_farmable, i.signature))
        tier2b = [i for i in ItemObject.every
                  if i.is_boss_loot and i not in tier0 + tier1]
        tier2b = sorted(tier2b,
                        key=lambda i: (i.is_boss_loot, i.signature))
        tier2c = [i for i in ItemObject.every
                  if i.is_chest and i not in tier0 + tier1]
        tier2c = sorted(tier2c, key=lambda i: (i.is_chest, i.signature),
                        reverse=True)
        tier2 = [i for i in ItemObject.every
                 if i in tier2b and i in tier2c]
        tier2 = sorted(
            tier2, key=lambda i: ((tier2b.index(i)/(len(tier2b)-1)) +
                                  (tier2c.index(i)/(len(tier2c)-1)),
                                  i.signature))
        tier3 = [i for i in ItemObject.every
                 if i in tier2b + tier2c and i not in tier2]

        def t3_sorter(i):
            mylist = tier2b if i in tier2b else tier2c
            return (mylist.index(i) / (len(mylist)-1), i.signature)

        tier3 = sorted(tier3, key=t3_sorter)
        tier4 = [i for i in ItemObject.every if i.is_legit and
                 i not in tier0 + tier1 + tier2 + tier3]
        tier4 = sorted(tier4, key=lambda i: i.signature)

        full_list = tier0 + tier1 + tier2 + tier3 + tier4

        assert len(full_list) == len(set(full_list))
        full_list = [i for i in full_list if i.is_legit]

        for i in full_list:
            i._rank_no_colosseum = full_list.index(i)

        for i in full_list:
            i._rank = i._rank_no_colosseum
            if i.is_colosseum:
                colosseum_rank = min(i2._rank_no_colosseum
                                     for i2 in i.is_colosseum)
                colosseum_rank = max(
                    colosseum_rank, max(i2._rank_no_colosseum
                                        for i2 in full_list if i2.is_buyable))
                if 0 < colosseum_rank < i._rank_no_colosseum:
                    i._rank = (colosseum_rank + i._rank_no_colosseum) / 2

        full_list = sorted(full_list, key=lambda i: (i._rank, i.signature))

        if 'BNW_2' in get_global_label():
            tierlist = path.join(tblpath, 'item_tiers_bnw2.txt')
            tiers = {}
            with open(tierlist) as f:
                for line in f:
                    tier, index, name, price = line.split()
                    index = int(index, 0x10)
                    tier = int(tier)
                    tiers[index] = tier
            max_value = max(tiers.values()) + 1
            for i in ItemObject.every:
                if i.is_legit and i.index not in tiers:
                    if i.is_legit:
                        tiers[i.index] = max_value
                    else:
                        tiers[i.index] = -1

            full_list = sorted(
                full_list, key=lambda i: (tiers[i.index], i._rank,
                                          i.signature))

        for i in full_list:
            i._rank = full_list.index(i)

        for i in ItemObject.every:
            if i.index in BANNED_INDEXES or not hasattr(i, '_rank'):
                i._rank = -1

        return self.rank

    @cached_property
    def highest_rank_price(self):
        return max([i.old_data['price'] for i in ItemObject.every
                    if i.rank <= self.rank and i.rank >= 0])

    @cached_property
    def is_buyable(self):
        for s in ShopObject.every:
            if set(s.old_data['item_ids']) <= {0x00, 0xFF}:
                continue
            if self.index in s.old_data['item_ids']:
                if 'BNW' in get_global_label():
                    for name in sorted(self.bnw_price_correction):
                        if name in self.name:
                            self.price = self.bnw_price_correction[name]
                            break
                return self.price
        return 0

    @cached_property
    def is_farmable(self):
        mls = []
        for ml in MonsterLootObject.every:
            if (ml.is_farmable and self.index in ml.old_data['steal_item_ids']
                    + ml.old_data['drop_item_ids']):
                mls.append(ml)
        if mls:
            return min([ml.monster.rank for ml in mls])
        return 0

    @cached_property
    def is_boss_loot(self):
        mls = []
        for ml in MonsterLootObject.every:
            if (not ml.is_farmable and
                    self.index in ml.old_data['steal_item_ids']
                    + ml.old_data['drop_item_ids']):
                mls.append(ml)
        if mls:
            return min([ml.monster.rank for ml in mls])
        return 0

    @cached_property
    def is_chest(self):
        cs = []
        for c in ChestObject.every:
            if self is c.old_treasure:
                cs.append(c)
        for c in CharacterObject.every[:14]:
            if self in c.old_initial_equipment:
                return 999
        return len(cs)

    @cached_property
    def is_initial_equipment(self):
        for c in CharacterObject.every[:14]:
            if self in c.old_initial_equipment:
                return True
        return False

    @property
    def is_colosseum(self):
        if hasattr(self, '_is_colosseum'):
            return self._is_colosseum

        for i in ItemObject.every:
            i._is_colosseum = set([])

        for c in ColosseumObject.every:
            if c.is_legit:
                c.trade._is_colosseum.add(c.item)

        while True:
            finished = True
            for i in ItemObject.every:
                for i2 in list(i._is_colosseum):
                    if i2._is_colosseum - i._is_colosseum:
                        i._is_colosseum |= i2._is_colosseum
                        finished = False
            if finished:
                break

        return self.is_colosseum

    @property
    def command_changes(self):
        matches = []
        for c in CmdChangeFBObject.every:
            if c.equipment_bit & self.special1:
                matches.append(c)
        return matches

    @property
    def mutate_valid(self):
        return self.is_equipable

    def mutate(self):
        if not self.mutate_valid:
            return
        if EquipmentStatsObject.flag not in get_flags():
            return
        super(ItemObject, self).mutate()
        if self.pretty_type == 'weapon':
            candidates = [o for o in self.every if o.pretty_type == 'weapon']
        else:
            candidates = [o for o in self.every if o.pretty_type != 'weapon']
        candidates = [o for o in candidates if o.mutate_valid]
        values = [o.hitmdef for o in candidates]
        minval, maxval = min(values), max(values)
        self.hitmdef = mutate_normal(self.hitmdef, minval, maxval,
                                     random_degree=self.random_degree)
        candidates = [o for o in self.every if o.mutate_valid]
        make_negative = lambda v: -(v & 0x7) if v >= 8 else v
        reverse_negative = lambda v: (0x8 | abs(v)) if v < 0 else v
        for attribute in ['speedvigor', 'magstam']:
            myval = make_negative(getattr(self, attribute) & 0xF)
            myval = mutate_normal(myval, -7, 7,
                                  random_degree=self.random_degree)
            myval = reverse_negative(myval)
            setattr(self, attribute,
                    (getattr(self, attribute) & 0xF0) | myval)

            myval = make_negative(getattr(self, attribute) >> 4)
            myval = mutate_normal(myval, -7, 7,
                                  random_degree=self.random_degree)
            myval = reverse_negative(myval)
            setattr(self, attribute,
                    (getattr(self, attribute) & 0xF) | (myval << 4))

        blockranks = [0xa, 0x9, 0x8, 0x7, 0x6, 0x0, 0x1, 0x2, 0x3, 0x4, 0x5]
        myval = self.mblockevade & 0xF
        index = mutate_normal(blockranks.index(myval), 0, len(blockranks)-1,
                              random_degree=self.random_degree)
        self.mblockevade = (self.mblockevade & 0xF0) | blockranks[index]

        myval = self.mblockevade >> 4
        index = mutate_normal(blockranks.index(myval), 0, len(blockranks)-1,
                              random_degree=self.random_degree)
        self.mblockevade = (self.mblockevade & 0xF) | (blockranks[index] << 4)

    @property
    def is_equipable(self):
        equiptypes = ['weapon', 'armor', 'shield', 'helm', 'relic']
        return (self.pretty_type in equiptypes and self.rank >= 0
                and self.old_data['equipability'] & 0x3fff)

    def magic_mutate_bits(self):
        if not self.is_equipable:
            return

        equiptypes = ['weapon', 'armor', 'shield', 'helm', 'relic']
        if not hasattr(ItemObject, 'character_mapping'):
            self.class_reseed('mut_equips')
            ItemObject.character_mapping = {}
            for equiptype in equiptypes:
                if equiptype not in self.character_mapping:
                    shuffled = list(range(14))
                    random.shuffle(shuffled)
                    self.character_mapping[equiptype] = shuffled

        equipability = self.equipability & 0x3fff
        random_degree = EquipabilityObject.random_degree
        if (bin(equipability).count('1') >= 2
                and random.random() < ((random_degree**0.5)
                                       *self.ranked_ratio)):
            equipables = [i for i in ItemObject.every if i.is_equipable]
            chosen = random.choice(equipables)
            if chosen is not self and (
                    chosen.old_data['equipability'] & equipability ==
                    chosen.old_data['equipability'] & 0x3fff):
                self.equipability = chosen.old_data['equipability']

        old_equip = self.equipability
        super(ItemObject, self).magic_mutate_bits()
        if self.pretty_type == 'weapon':
            for i in range(14):
                mask = 1 << i
                xor = old_equip ^ self.equipability
                if (mask & xor and random.random()**2 > random_degree):
                    self.equipability = (self.equipability | mask) ^ mask
                    self.equipability |= (old_equip & mask)

        if self.is_equipable and not self.equipability & 0x3fff:
            self.equipability |= self.old_data['equipability'] & 0x3fff

        self.equipability = (self.equipability | 0x4000) ^ 0x4000
        self.equipability |= (self.old_data['equipability'] & 0x4000)

        if not self.is_equipable or 'c' not in get_flags():
            return

        mutated = self.equipability
        self.equipability &= 0xC000
        for (i, c) in enumerate(self.character_mapping[self.pretty_type]):
            if mutated & (1 << i):
                self.equipability |= (1 << c)

    def cleanup(self):
        if self.pretty_type not in ['armor', 'shield', 'helm', 'relic']:
            for attribute in ['elements', 'elemabsorbs',
                              'elemnulls', 'elemweaks']:
                setattr(self, attribute, self.old_data[attribute])
            if 'q' in get_flags():
                self.set_bit('swdtech', True)
                self.set_bit('runic_percentage', True)

        if not self.is_equipable:
            attrs = [a for (a, b, c) in self.specs.attributes]
            assert 'price' in attrs
            for a in attrs:
                if a != 'price':
                    setattr(self, a, self.old_data[a])

        if self.price > 100 and 'i' in get_flags():
            price = self.price * 2
            counter = 0
            while price >= 100:
                price //= 10
                counter += 1
            price *= (10**counter)
            self.price = price // 2

        if self.price < 10 and self.old_data['price'] <= 2:
            self.price = self.old_data['price']

        if self.is_equipable and 'q' in get_flags():
            assert (self.equipability & 0x4000 ==
                    self.old_data['equipability'] & 0x4000)
            if self.command_changes:
                equip_mask = 0
                for cc in self.command_changes:
                    for c in CharacterObject.every[:14]:
                        if cc.command in c.commands:
                            equip_mask |= (1 << c.index)
                equip_mask |= 0x1000
                self.equipability = equip_mask

            if self.equipability & 0xbfff == 0xbfff:
                self.equipability ^= 0x8000

        if 'BNW' in get_global_label() and 'Moogle_Charm' in self.name:
            self.equipability |= 0x3fff

        if (self.learnrate > 0 and 'a' in get_flags() and 'q' in get_flags()
                and self.is_equipable and self.pretty_type != 'weapon'):
            equipability = self.equipability & 0xfff
            learnability = 0
            for i in range(14):
                spells = CharEsperObject.get_character_spells(i)
                if self.learnspell in spells:
                    learnability |= (1 << i)
            both = equipability & learnability
            if both:
                self.equipability &= 0xf000
                self.equipability |= both
            elif bin(equipability).count('1') > bin(learnability).count('1'):
                self.equipability &= 0xf000
                self.equipability |= learnability

        if 'fanatix' in get_activated_codes():
            if self.index in [0xF6, 0xF7]:
                self.price = 0
                self.otherproperties = 0
                self.itemtype = 6


class EsperObject(TableObject):
    flag = 'e'
    flag_description = 'espers'
    custom_random_enable = True

    randomize_attributes = ['bonus']

    def __repr__(self):
        s = 'ESPER %x\n' % self.index
        for i in range(1, 6):
            s += '{0:0>2}'.format('%x' % getattr(self, 'spell%s' % i))
            s += ' x%s\n' % getattr(self, 'learn%s' % i)
        return s.strip()

    @property
    def intershuffle_valid(self):
        return self.valid

    @property
    def valid(self):
        if 'BNW' in get_global_label() and (self.index == 0x42-0x36):
            return False
        return True

    @property
    def spell_object(self):
        return SkillObject.get(0x36 + self.index)

    def get_spell_similarity_score(self, spell):
        if not hasattr(EsperObject, '_spell_similarity_averages'):
            EsperObject._spell_similarity_averages = {}
            for s in SkillObject.every[:0x36]:
                scores = []
                for e in EsperObject.every:
                    scores.append(e.spell_object.get_bit_similarity_score(s))
                EsperObject._spell_similarity_averages[s] = (
                    sum(scores) / float(len(scores)))
        score1 = (self.spell_object.get_bit_similarity_score(spell)
                  - EsperObject._spell_similarity_averages[spell])
        scores = []
        for s, l in self.old_spell_learns:
            if s == 0xFF:
                continue
            s = SkillObject.get(s)
            scores.append(s.get_bit_similarity_score(spell))
        if not scores:
            return score1

        score2 = (sum(scores) / float(len(scores)) -
                  spell.get_bit_similarity_score(spell))
        return (score1 + score1 + score2) / 3.0

    @cached_property
    def old_spell_learns(self):
        spell_learns = []
        for i in range(1, 6):
            spell_learns.append(
                (self.old_data['spell%s' % i], self.old_data['learn%s' % i]))
        return spell_learns

    @property
    def spell_learns(self):
        spell_learns = []
        for i in range(1, 6):
            spell_learns.append(
                (getattr(self, 'spell%s' % i), getattr(self, 'learn%s' % i)))
        return spell_learns

    @classproperty
    def spell_freq(cls):
        if hasattr(EsperObject, '_spell_freq'):
            return EsperObject._spell_freq

        num_spells, total = 0, 0
        for e in EsperObject.every:
            if not e.valid:
                continue
            for s, l in e.old_spell_learns:
                if s <= 0x35:
                    num_spells += 1
                total += 1

        EsperObject._spell_freq = float(num_spells) / total
        return EsperObject.spell_freq

    @property
    def ranked_spell_candidates(self):
        candidates = sorted(
            SkillObject.every[:0x36],
            key=lambda s: (self.get_spell_similarity_score(s),
                           s.signature, s.index), reverse=True)
        return candidates

    @classmethod
    def randomize_all(cls):
        done_spells = set([])
        for i in range(5):
            EsperObject.class_reseed('esper_spells%s' % i)
            espers = list(EsperObject.every)
            random.shuffle(espers)
            for e in espers:
                if not hasattr(e, 'new_spells'):
                    e.new_spells = []
                if not e.valid:
                    continue
                e.reseed('esper_spell%s' % i)
                if random.random() > EsperObject.spell_freq:
                    continue
                candidates = [c.index for c in e.ranked_spell_candidates]
                candidates = [c for c in candidates if c not in e.new_spells]
                candidates = ([c for c in candidates if c not in done_spells] +
                              [c for c in candidates if c in done_spells])
                max_index = len(candidates)-1
                index = mutate_normal(0, 0, max_index, wide=True,
                                      random_degree=e.random_degree**2)
                chosen = candidates[index]
                e.new_spells.append(chosen)
                done_spells.add(chosen)

        for e in EsperObject.every:
            e.reseed('esper_learn')
            e.randomize()
            while len(e.new_spells) < 5:
                e.new_spells.append(0xFF)
            e.new_spells = sorted(e.new_spells)
            for i in range(5):
                setattr(e, 'spell%s' % (i+1), e.new_spells[i])
                setattr(e, 'learn%s' % (i+1),
                        e.make_spell_learn_rate(e.new_spells[i]))
            e.randomized = True

    @classmethod
    def make_spell_learn_rate(cls, spell_id):
        if spell_id == 0xFF:
            return 0

        old_learn_rates = []
        lowest = 0xFF
        highest = 0
        for e in EsperObject.every:
            for s, l in e.old_spell_learns:
                lowest = min(lowest, l)
                highest = max(highest, l)
                if s == spell_id:
                    old_learn_rates.append(l)

        if not old_learn_rates:
            if 'BNW' in get_global_label():
                return highest
            else:
                return lowest

        chosen = random.choice(old_learn_rates)
        chosen = mutate_normal(chosen, lowest, highest,
                               random_degree=EsperObject.random_degree)
        if chosen >= 10:
            chosen = int(round(chosen*2, -1) / 2)
        elif chosen == 9:
            chosen = 8
        elif chosen == 6:
            chosen = 7
        return chosen


class CmdNameObject(TableObject): pass


class ShopPaletteObject(TableObject):
    @property
    def char_palette(self):
        return CharPaletteObject.get(self.index)

    def cleanup(self):
        indexes = {getattr(self, 'index%s' % i) for i in range(4)}
        assert len(indexes) == 1
        indexes = list(indexes)
        converted_index = ((indexes[0] & 0xe) >> 1) - 2
        assert (self.char_palette.old_data['palette_index'] == converted_index)
        new_converted_index = (self.char_palette.palette_index + 2) << 1
        new_converted_index |= indexes[0] & 0xF1
        for i in range(4):
            setattr(self, 'index%s' % i, new_converted_index)


class FormationAPObject(TableObject): pass

class ColosseumObject(TableObject):
    def __repr__(self):
        return '%s -> %s : %s' % (
            self.item.name, self.trade.name, self.opponent.name)

    @property
    def is_legit(self):
        if 'JP' in get_global_label():
            return self.opponent.index != 0x40
        return 'chupon' not in str(self).lower()

    @property
    def opponent(self):
        return MonsterObject.get(self.opponent_id)

    @property
    def item(self):
        return ItemObject.get(self.index)

    @property
    def trade(self):
        return ItemObject.get(self.trade_id)


class EntranceObject(TableObject): pass


class NPCPaletteObject(PaletteMixin):
    flag = 'l'
    flag_description = 'character palettes'

    SKINTONES = [((31, 24, 17), (25, 13, 7)),
                 ((31, 23, 15), (25, 15, 8)),
                 ((31, 24, 17), (25, 13, 7)),
                 ((31, 25, 15), (25, 19, 10)),
                 ((31, 25, 16), (24, 15, 12)),
                 ((27, 17, 10), (20, 12, 10)),
                 ((25, 20, 14), (19, 12, 4)),
                 ((27, 22, 18), (20, 15, 12)),
                 ((28, 22, 16), (22, 13, 6)),
                 ((28, 23, 15), (22, 16, 7)),
                 ((27, 23, 15), (20, 14, 9))]
    CHAR_HUES = [0, 10, 20, 30, 45, 60, 75, 90, 120, 150, 180,
                 200, 220, 240, 270, 300, 330, 345]

    def randomize(self):
        if self.pointer == addresses.npc_choco_palette_address:
            while True:
                hue_offset = (random.random() * 360) - 180
                farness = abs(hue_offset) / 30
                if farness >= 1 or random.random() < farness:
                    break
            self.recolor_by_cluster(
                'all', hue_randomization='offset', hue_offset=hue_offset)
            self._choco_offset = hue_offset
            return

        if 6 <= self.index <= 7:
            return

        if not hasattr(NPCPaletteObject, '_shuffled'):
            random.shuffle(NPCPaletteObject.SKINTONES)
            NPCPaletteObject.CHAR_HUES = shuffle_char_hues(
                NPCPaletteObject.CHAR_HUES)
            NPCPaletteObject._shuffled = True
        before = (len(NPCPaletteObject.SKINTONES),
                  len(NPCPaletteObject.CHAR_HUES))
        trance = self.index == 8
        colors = generate_character_palette(
            NPCPaletteObject.SKINTONES, NPCPaletteObject.CHAR_HUES, trance)
        after = (len(NPCPaletteObject.SKINTONES),
                 len(NPCPaletteObject.CHAR_HUES))
        if not trance:
            assert after[0] == before[0] - 1
            assert after[1] == before[1] - 3
        self.colors[:12] = colors

        npc_only_cluster = [n for (n, c) in enumerate(self.colors)][12:]
        self.recolor_by_cluster(clusters=[npc_only_cluster],
                                hue_randomization='random')

    def cleanup(self):
        assert len(self.colors) == len(self.old_data['colors'])


class MiscPaletteObject(PaletteMixin):
    def cleanup(self):
        if NPCPaletteObject.flag in get_flags():
            for npcpo in NPCPaletteObject.every:
                if hasattr(npcpo, '_choco_offset'):
                    self.recolor_by_cluster('all', 'offset',
                                            npcpo._choco_offset)


class LocNamePtrObject(TableObject): pass

class BBGPaletteObject(PaletteMixin):
    def shift_blue(self):
        for i in range(len(self.colors)):
            color = self.colors[i]
            r, g, b = self.color_to_rgb(color)
            r, g, b = b, g, r
            h, s, l = self.rgb_to_hsl(r, g, b)
            if l < 0.2:
                s = s * ((l*5)**2)
            self.set_color_hsl(i, h, s, l)


class NatMagCharObject(TableObject):
    flag = 'o'

    @classproperty
    def after_order(self):
        return [CharacterObject]

    def randomize(self):
        candidates = [c.index for c in CharacterObject.every
                      if c.index < 12 and set(c.commands) & {2, 0x17}]
        if self.index != 0:
            first = NatMagCharObject.get(0)
            if first.character_index in candidates:
                candidates.remove(first.character_index)
        self.character_index = random.choice(candidates)

    def cleanup(self):
        assert self.old_data['character_index'] in {0, 6}
        other = NatMagCharObject.get(self.index ^ 1)
        assert self.character_index != other.character_index
        with open(get_outfile(), 'r+b') as f:
            if 'JP' in get_global_label():
                pointer = 0xa0b8 + (4 * self.index)
            else:
                pointer = 0xa183 + (4 * self.index)
            f.seek(pointer-1)
            validate = f.read(2)
            expected = b'\xc9' + bytes([self.old_data['character_index']])
            assert validate == expected
            f.seek(pointer)
            f.write(bytes([self.character_index]))
            if 'JP' in get_global_label():
                pointer = 0xa0e1 + (34 * self.index)
            else:
                pointer = 0xa1ac + (34 * self.index)
            address = 0x1a6e + (54 * self.character_index)
            #f.seek(pointer)
            #old_address = int.from_bytes(f.read(2), byteorder='little')
            f.seek(pointer)
            f.write(address.to_bytes(2, byteorder='little'))


class NaturalMagicMixin(TableObject):
    flag = 'o'
    custom_random_enable = True

    def randomize(self):
        candidates = [s for s in SkillObject.every if s.index <= 0x35]
        for nmo in self.every:
            if hasattr(nmo, 'randomized') and nmo.randomized:
                if (self.level == nmo.level
                        and self.spell == nmo.old_data['spell']):
                    self.spell = nmo.spell
                    return
                candidates = [s for s in candidates if s.index != nmo.spell]
        old_spell = SkillObject.get(self.spell)
        new_spell = old_spell.get_similar(
            candidates=candidates, override_outsider=True,
            random_degree=self.random_degree)
        assert new_spell in candidates
        self.spell = new_spell.index


class TerraNatMagObject(NaturalMagicMixin): pass
class CelesNatMagObject(NaturalMagicMixin): pass


class WeaponAnimObject(TableObject): pass
class WindowGfxObject(TableObject): pass


class WindowPaletteObject(PaletteMixin):
    def randomize(self):
        self.recolor_by_cluster(hue_randomization='random')

    def cleanup(self):
        if (MonsterPaletteObject.flag not in get_flags()
                and NPCPaletteObject.flag not in get_flags()):
            self.colors = self.old_data['colors']


class BattlePaletteObject(TableObject):
    @property
    def npc_palette(self):
        if self.index <= 5:
            return NPCPaletteObject.get(self.index)
        elif self.index == 6:
            return NPCPaletteObject.get(8)

    def cleanup(self):
        battle_colors = self.old_data['colors'][1:12]
        npc_colors = self.npc_palette.old_data['colors'][1:12]
        num_same = len([1 for (a, b) in zip(battle_colors, npc_colors)
                        if a == b])
        assert num_same >= (len(battle_colors)-1)
        self.colors[1:12] = self.npc_palette.colors[1:12]


class CharacterObject(TableObject):
    flag = 'c'
    flag_description = 'characters'
    custom_random_enable = True

    randomize_attributes = [
        'hp', 'mp', 'vigor', 'speed', 'stamina', 'magpwr',
        'batpwr', 'def', 'magdef', 'evade', 'mblock',
        ]
    mutate_attributes = {
        'hp': None,
        'mp': None,
        'vigor': None,
        'speed': None,
        'stamina': None,
        'magpwr': None,
        'batpwr': None,
        'def': None,
        'magdef': None,
        'evade': None,
        'mblock': None,
        }

    @classproperty
    def after_order(cls):
        return [CmdChangeFBObject]

    @property
    def intershuffle_valid(self):
        return self.index < 12

    @property
    def name(self):
        return to_ascii(CharNameObject.get(self.index).name_text)

    @property
    def old_initial_equipment_ids(self):
        return ([self.old_data[attr]
                 for attr in ['weapon', 'shield', 'helm', 'armor']]
                + self.old_data['relics'])

    @property
    def old_initial_equipment(self):
        return [ItemObject.get(i)
                for i in self.old_initial_equipment_ids if i <= 0xFE]

    @classproperty
    def valid_commands(cls):
        if hasattr(CharacterObject, '_valid_commands'):
            return CharacterObject._valid_commands

        valid_commands = set([])
        seen_commands = set([])
        for char in CharacterObject.every:
            for c in char.commands:
                if c <= 0x1f:
                    if char.index < 14:
                        valid_commands.add(c)
                    seen_commands.add(c)
        for c in CmdChangeFBObject.every:
            if c.command in valid_commands:
                command = CmdChangeTBObject.get(c.index).command
                valid_commands.add(command)
                seen_commands.add(command)
        for command in [0x19, 0x1a, 0x1b]:
            if command in seen_commands:
                valid_commands.add(command)
        if 'BNW_2' not in get_global_label():
            valid_commands.add(0x1d)
        for command in [0x00, 0x01, 0x02, 0x11, 0xff]:
            if command in valid_commands:
                valid_commands.remove(command)
        CharacterObject._valid_commands = sorted(valid_commands)

        return CharacterObject.valid_commands

    @classproperty
    def current_initial_commands(cls):
        seen_commands = set([])
        for char in CharacterObject.every:
            if char.index < 14:
                seen_commands |= set(char.commands)
        if 0xFF in seen_commands:
            seen_commands.remove(0xFF)
        return seen_commands

    def randomize_commands(self):
        if 'o' not in get_flags() or self.index in [12, 13]:
            return

        if not hasattr(CharacterObject, '_done_commands'):
            CharacterObject._done_commands = set([0, 2, 1])

        if self.index > 13 or 'wildcommands' not in get_activated_codes():
            commands = [c for c in self.old_data['commands']
                        if c in [0x00, 0x02, 0x11, 0x01]]
            if 0x11 in commands and 0x00 not in commands:
                commands.remove(0x11)
                commands.insert(0, 0)
        else:
            while True:
                commands = []
                if random.choice([True, True, False]):
                    commands.append(0x00)
                if random.choice([True, True, False]):
                    commands.append(0x02)
                if random.choice([True, True, False]):
                    commands.append(0x01)
                if 0x00 in commands or 0x01 in commands:
                    break

        if not commands:
            return

        while len(commands) < 4:
            valid_commands = CharacterObject.valid_commands
            if (self.index <= 0xc
                    and 'wildcommands' not in get_activated_codes()):
                valid_commands = [c for c in valid_commands
                                  if c not in CharacterObject._done_commands]
            chosen = random.choice(valid_commands)
            if chosen not in commands:
                if commands[0] == 0x00:
                    commands.insert(1, chosen)
                else:
                    commands.insert(0, chosen)

        NO_RAGE_PATCH = True
        for patchfilename in get_activated_patches():
            if 'auto_learn_rage_patch' in patchfilename.lower():
                NO_RAGE_PATCH = False
                break
        else:
            if 'BNW' in get_global_label():
                write_patch(get_outfile(), 'patch_auto_learn_rage.txt')
                NO_RAGE_PATCH = False

        if self.index == 0x0b and 0x11 not in commands and NO_RAGE_PATCH:
            replaceable = [c for c in commands if c not in [0x10, 0x00, 0x01]]
            commands.remove(random.choice(replaceable))
            commands.insert(1, 0x11)

        self.commands = commands
        if self.index <= 0xc:
            CharacterObject._done_commands |= set(self.commands)

    def randomize(self):
        self.reseed('rand_escape')
        if self.index < 14:
            c = CharacterObject.get(random.randint(0, 14))
            escape = c.old_data['level_escape'] & 3
            c.level_escape = (c.level_escape & 0xFC) | escape
        super(CharacterObject, self).randomize()

    def cleanup(self):
        if self.index < 14:
            for attr in ['weapon', 'shield', 'helm', 'armor']:
                index = getattr(self, attr)
                if index == 0xFF:
                    continue
                item = ItemObject.get(index)
                if not item.equipability & (1 << self.index):
                    candidates = [
                        i for i in ItemObject.ranked if i.pretty_type == attr
                        and i.rank >= 0
                        and (i.is_buyable or i.is_initial_equipment)
                        and i.equipability & (1 << self.index)]
                    if candidates:
                        setattr(self, attr, candidates[0].index)
                    else:
                        setattr(self, attr, 0xff)
            for (i, r) in enumerate(self.relics):
                if r == 0xFF:
                    continue
                item = ItemObject.get(r)
                if not item.equipability & (1 << self.index):
                    self.relics[i] = 0xFF

        if 'fanatix' in get_activated_codes():
            self.level_escape &= 0xF3
            if self.index == 0x0E:
                if get_global_label() not in ['FF6_NA_1.0', 'FF6_NA_1.1',
                                              'FF6_JP']:
                    self.level_escape |= 0x08
                self.relics = [0xFF, 0xFF]
                for attr in ['weapon', 'shield', 'helm', 'armor']:
                    setattr(self, attr, 0xFF)

        if self.index == 0x1b and 't' in get_flags():
            self.weapon = 0x29


class ExperienceObject(TableObject):
    def cleanup(self):
        if 'fanatix' in get_activated_codes():
            self.experience //= 2


class ChestObject(TableObject):
    flag = 't'
    flag_description = 'treasure'
    custom_random_enable = True

    @property
    def memid(self):
        memid = self.memid_low
        if self.get_bit('memid_high'):
            memid |= 0x100
        return memid

    @property
    def mutate_valid(self):
        return self.rank is not None and self.rank >= 0

    def set_memid(self, index):
        assert index <= 0x1FF
        if self.index & 0x100:
            self.set_bit('memid_high', True)
        else:
            self.set_bit('memid_high', False)
        self.memid_low = index & 0xFF

    @cached_property
    def old_treasure(self):
        if self.old_data['misc'] & 0x40:
            return ItemObject.get(self.old_data['contents'])
        return None

    @cached_property
    def old_formation(self):
        if self.old_data['misc'] & 0x20:
            return TwoPackObject.get(self.old_data['contents'])
        return None

    @cached_property
    def rank(self):
        item = None
        if self.old_treasure:
            item = self.old_treasure
        if self.old_formation:
            item = self.old_formation.guaranteed_treasure
            if item is None:
                rank = (self.old_formation.ranked_ratio**2
                        if self.old_formation.ranked_ratio is not None else -1)
                return rank

        if item:
            return item.ranked_ratio if item.ranked_ratio is not None else -1

        if self.old_data['misc'] & 0x80:
            return self.old_data['contents'] / 655.35
        else:
            return 0

    def set_contents(self, item):
        for bitname in ['gold', 'treasure', 'monster', 'empty1', 'empty2']:
            self.set_bit(bitname, False)
        if isinstance(item, ItemObject):
            self.set_bit('treasure', True)
            item = item.index
        elif isinstance(item, TwoPackObject):
            self.set_bit('monster', True)
            item = item.index
        elif isinstance(item, int):
            self.set_bit('gold', True)
        else:
            raise NotImplementedError
        self.contents = item

    def mutate(self):
        if not hasattr(ChestObject, '_done_miabs'):
            ChestObject._done_miabs = set()

        candidates = [c for c in ChestObject.every if c.rank > 0]
        chosen_type = random.choice(candidates)
        if chosen_type.old_treasure:
            candidates = ItemObject.ranked
        elif chosen_type.old_formation:
            if chosen_type.old_formation.guaranteed_treasure:
                candidates = [tp for tp in TwoPackObject.ranked
                              if tp.guaranteed_treasure]
            else:
                candidates = TwoPackObject.ranked
            candidates = [c for c in candidates
                          if c not in ChestObject._done_miabs]
            if not candidates:
                chosen_type = [c for c in ChestObject.every
                               if c.old_treasure][0]
                candidates = ItemObject.ranked
        else:
            # gold
            candidates = None
            value = (self.ranked_ratio**2) * 255
            value = mutate_normal(value, 0, 255, wide=True,
                                  random_degree=self.random_degree)
            value = min(255, int(round(value)))
            self.set_contents(value)
            return

        candidates = [c for c in candidates
                      if c.rank >= 0 and c.ranked_ratio is not None]
        max_index = len(candidates)-1
        if chosen_type.old_treasure:
            index = len([c for c in candidates
                         if c.ranked_ratio <= self.ranked_ratio]) - 1
        else:
            index = len([c for c in candidates
                         if c.ranked_ratio <= self.ranked_ratio**0.5]) - 1
        index = max(index, 0)
        index = mutate_normal(index, 0, max_index, wide=True,
                              random_degree=self.random_degree)
        chosen = candidates[index]
        if isinstance(chosen, TwoPackObject):
            ChestObject._done_miabs.add(chosen)
        self.set_contents(chosen)

    @classmethod
    def mutate_all(cls):
        ChestObject.class_reseed('chests')
        ordering = list(ChestObject.every)
        random.shuffle(ordering)
        for o in ordering:
            if hasattr(o, "mutated") and o.mutated:
                continue
            o.reseed(salt="mut")
            if o.mutate_valid:
                o.mutate()
            o.mutated = True

    def cleanup(self):
        if not hasattr(self, 'old_data'):
            return

        for c in ChestObject.every:
            if c.index >= self.index:
                break
            if not hasattr(c, 'old_data'):
                continue
            if (c.memid == self.memid
                    and c.old_data['misc'] == self.old_data['misc']
                    and c.old_data['contents'] == self.old_data['contents']):
                self.misc = c.misc
                self.contents = c.contents
                break


class LocationObject(TableObject):
    @property
    def events(self):
        return EventObject.getgroup(self.index)

    @property
    def npcs(self):
        return NpcObject.getgroup(self.index)

    @property
    def exits(self):
        return EntranceObject.getgroup(self.index)

    @property
    def long_exits(self):
        return LongEntranceObject.getgroup(self.index)

    @property
    def chests(self):
        return ChestObject.getgroup(self.index)

    @property
    def area_pack(self):
        return AreaPackObject.get(self.index)

    @property
    def pack(self):
        return self.area_pack.pack

    def set_enemy_pack(self, pack):
        self.area_pack.pack_id = pack.index & 0xFF
        assert self.pack is pack

    @property
    def formations(self):
        return self.pack.formations

    def purge_associated_objects(self):
        for x in self.exits + self.long_exits:
            x.groupindex = -1
        for e in self.events:
            e.groupindex = -1
        for n in self.npcs:
            n.groupindex = -1

    def set_palette(self, value):
        self.palette_index |= 0x3F
        self.palette_index ^= 0x3F
        self.palette_index |= value

    def reload_randomized_music(self):
        new_data = dict(self.old_data)
        self.read_data()
        new_music = self.music
        for attr, value in new_data.items():
            setattr(self, attr, value)
        self.music = new_music


class FieldPaletteObject(TableObject): pass
class LongEntranceObject(TableObject): pass

class CharEsperObject(TableObject):
    flag = 'a'
    flag_description = 'esper allocations'

    @classproperty
    def esper_names(cls):
        return ['Ramuh', 'Ifrit', 'Shiva', 'Siren', 'Terrato', 'Shoat',
                'Maduin', 'Bismark', 'Stray', 'Palador', 'Tritoch', 'Odin',
                'Raiden', 'Bahamut', 'Alexandr', 'Crusader', 'Ragnarok',
                'Kirin', 'ZoneSeek', 'Carbunkl', 'Phantom', 'Sraphim', 'Golem',
                'Unicorn', 'Fenrir', 'Starlet', 'Phoenix']

    def __repr__(self):
        if len(CharEsperObject.every) <= 16:
            s = ''
            for i in range(len(self.esper_names)):
                if self.allocations & (1 << i):
                    s += ' ' + self.esper_names[i]
            return '%x: %s' % (self.index, s.strip())
        else:
            s = '%x %s: ' % (self.index, self.esper_names[self.index])
            for i in range(16):
                if self.allocations & (1 << i):
                    s += '%s ' % i
            return s.strip()

    def cleanup(self):
        if 'BNW' not in get_global_label() and 'a' not in get_flags():
            if len(CharEsperObject.every) <= 16:
                self.allocations = 0xFFFFFFFF
            else:
                self.allocations = 0xFFFF
        if 'a' in get_flags() and ('BNW' not in get_global_label()
                or self.old_data['allocations']):
            assert self.allocations > 0

    @classproperty
    def allocations_by_character(self):
        if len(CharEsperObject.every) <= 16:
            allocations = [ceo.allocations for ceo in CharEsperObject.every]
            while len(allocations) < 16:
                allocations.append(0)
            return allocations

        allocations = [0] * 16
        for ceo in CharEsperObject.every:
            for i in range(len(allocations)):
                if ceo.allocations & (1 << i):
                    allocations[i] |= (1 << ceo.index)
        return allocations

    @classmethod
    def get_character_spells(self, charid):
        allocations = self.allocations_by_character[charid]
        spells = set([])
        for i in range(len(CharEsperObject.esper_names)):
            if allocations & (1 << i):
                e = EsperObject.get(i)
                for s, l in e.spell_learns:
                    spells.add(s)
        if 0xff in spells:
            spells.remove(0xff)
        return sorted(spells)

    @classmethod
    def allocate(cls, character, esper):
        if len(CharEsperObject.every) <= 16:
            ceo = CharEsperObject.get(character)
            ceo.allocations |= (1 << esper)
        else:
            ceo = CharEsperObject.get(esper)
            ceo.allocations |= (1 << character)

    @classmethod
    def full_randomize(cls):
        values = set(CharEsperObject.allocations_by_character)
        char_ratios, esper_ratios = {}, {}
        if len(values) == 1:
            valid_mask = 0x7ffffff
        else:
            valid_mask = 0
            for v in values:
                valid_mask |= v

        for i in range(32):
            if valid_mask & (1 << i):
                esper_ratios[i] = 0.15
            else:
                esper_ratios[i] = 0.0

        for i in range(12):
            char_ratios[i] = 0.15
        for i in range(12, 16):
            char_ratios[i] = 0.0

        for ceo in CharEsperObject.every:
            ceo.allocations = 0

        for i in range(16):
            char_ratio = char_ratios[i]
            for k, esper_ratio in sorted(esper_ratios.items()):
                if min(char_ratio, esper_ratio) <= 0:
                    continue
                avg_ratio = (char_ratio + esper_ratio) / 2.0
                CharEsperObject.class_reseed('allocate %s %s' % (i, k))
                if random.random() < avg_ratio:
                    CharEsperObject.allocate(i, k)

        new_mask = 0
        for allocation in CharEsperObject.allocations_by_character:
            new_mask |= allocation

        undone_chars = [i for i in range(16) if char_ratios[i]
                        and not CharEsperObject.allocations_by_character[i]]
        undone_espers = [i for i in range(32) if esper_ratios[i]
                         and (valid_mask ^ new_mask) & (1 << i)]

        CharEsperObject.class_reseed('remaining_allocations')
        while undone_chars or undone_espers:
            if undone_chars:
                chosen_char = random.choice(undone_chars)
                undone_chars.remove(chosen_char)
            else:
                chosen_char = random.choice([i for (i, a) in enumerate(
                    CharEsperObject.allocations_by_character) if a])
            if undone_espers:
                chosen_esper = random.choice(undone_espers)
                undone_espers.remove(chosen_esper)
            else:
                chosen_esper = random.choice(
                    [i for i in range(32) if valid_mask & (1 << i)])
            CharEsperObject.allocate(chosen_char, chosen_esper)

        super(CharEsperObject, cls).full_randomize()


def number_location_names():
    if 'JP' in get_global_label():
        raise NotImplementedError
    pointer = addresses.location_names
    f = open(get_outfile(), 'r+b')
    f.seek(pointer)
    f.write(b'\x00')
    for i in range(1, 101):
        pointer = f.tell() - addresses.location_names
        LocNamePtrObject.get(i).name_pointer = pointer
        s = '{0:0>2}'.format(i)
        for c in s:
            v = int(c)
            f.write(bytes([0x54 + v]))
        f.write(b'\x00')
    assert f.tell() <= addresses.location_names_max
    f.close()


fanatix_space_pointer = None


def execute_fanatix_mode():
    if not FOOLS:
        print('FANATIX MODE ACTIVATED')

    for i in range(0x20):
        InitialMembitObject.get(i).membyte = 0xFF
    # bit needs to be unset for saving to work
    InitialMembitObject.set_membit(0xa0, value=False)
    assert InitialMembitObject.get(0x14).membyte == 0xFE

    BANNED_BBGS = [0x07, 0x0D, 0x19, 0x21, 0x25, 0x29, 0x2c,
                   0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36]
    BANNED_MAPS = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x09, 0x0B, 0x0C, 0x0D,
        0x11, 0x14, 0x15, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
        0x22, 0x29, 0x2f, 0x37, 0x40, 0x4b, 0x4e, 0x50, 0x53, 0x5b,
        0x60, 0x61, 0x70, 0x72, 0x73, 0x75, 0x7b, 0x7d, 0x7e, 0x7f,
        0x80, 0x81, 0x82, 0x83, 0x88, 0x89, 0x8c, 0x8f,
        0x90, 0x92, 0x96, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e,
        0xa2, 0xa3, 0xa5, 0xa7, 0xa9, 0xab, 0xad,
        0xb6, 0xb7, 0xb8, 0xbb, 0xbd, 0xbe, 0xbf,
        0xc0, 0xc1, 0xc2, 0xc4, 0xc5, 0xc9, 0xcd, 0xcf,
        0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd7, 0xd9, 0xdd,
        0xe1, 0xe7, 0xe9, 0xea, 0xeb,
        0xfd, 0xfe, 0xff,
        0x100, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x10c,
        0x110, 0x112, 0x114, 0x11c, 0x120, 0x121, 0x122, 0x12e,
        0x131, 0x132, 0x133, 0x134, 0x135, 0x136, 0x137,
        0x138, 0x139, 0x13a, 0x13b, 0x13c, 0x13d, 0x13e,
        0x140, 0x141, 0x142, 0x143, 0x144, 0x145, 0x146, 0x147,
        0x148, 0x14a, 0x14c,
        0x150, 0x154, 0x155,
        ]
    BANNED_MAPS += range(0x157, 0x200)

    for l in LocationObject.every:
        if l.index in BANNED_MAPS and l.index not in [0x16a, 0x16c]:
            continue
        l.name_id = 0
        for x in l.exits:
            if l.index > 2:
                x.groupindex = -1
        for x in l.long_exits:
            if l.index != 0x16a:
                x.groupindex = -1
        for c in l.chests:
            c.groupindex = -1

        for n in l.npcs:
            n.groupindex = -1
        for e in l.events:
            e.groupindex = -1

    if 'JP' not in get_global_label():
        number_location_names()

    opening_event = [
        0xB8, 0x42,                         # enable morph
        0xB8, 0x43,                         # show magic points after battle
        0xB8, 0x4B,                         # shadow can't leave
        0xD0, 0x53,                         # show MP
        0x88, 0x00, 0x00, 0x00,             # remove magitek status from terra
        0x3F, 0x00, 0x00,                   # remove terra
        0x3E, 0x00,
        0x3F, 0x0E, 0x00,                   # remove biggs
        0x3E, 0x0E,
        0x3F, 0x0F, 0x00,                   # remove wedge
        0x3E, 0x0F,
        ]
    for i in range(0xE):
        opening_event += [
            0x7F, i, i,     # character name
            0x37, i, i,     # character sprite
            0x43, i, CharPaletteObject.get(i).palette_index,
            0x40, i, i,     # character data
            0xD4, 0xE0+i,
            0xD4, 0xF0+i,
            ]

    if 'bonanza' in get_activated_codes():
        for i in range(27):   # espers
            opening_event += [0x86, i + 0x36,]

    opening_event += [
        0x80, 0xf0,
        0x80, 0xf0,
        ]

    opening_event += [
        0x7F, 0x0E, 0x0E,   # banon
        0x37, 0x0E, 0x11,
        0x43, 0x0E, 0x03,
        0x40, 0x0E, 0x0E,
        ]

    opening_event += [
        0x3D, 0x00,
        0x3F, 0x00, 0x01,                           # start with terra
        0x84, 0xB8, 0x0B,                           # starting gil
        0x6B, 0x01, 0x20, 160, 127, 0x00, 0xFF,     # start at fanatics tower
        0xFE,
        ]
    fo = open(get_outfile(), 'r+b')
    fo.seek(addresses.opening_crawl_pointer)
    fo.write(bytes([0xFD]*4))  # no opening crawl
    opening_jump_pointer = addresses.opening_jump_pointer
    fo.seek(addresses.opening_pointer)
    fo.write(bytes(
        [0xB2] + int_to_bytelist(opening_jump_pointer-0xA0000, 3) + [0xFE]))
    fo.seek(opening_jump_pointer)
    fo.write(bytes(opening_event))

    partydict, fulldict = {}, {}
    removedict, addict = {}, {}
    done_parties = set([])
    NUM_FLOORS = 99
    next_membit = 1
    LocationObject.class_reseed('prefanatix')
    for n in range(NUM_FLOORS):
        if n == 0:
            party = tuple(sorted(random.sample(range(14),5)))
            addict[n] = random.choice(party)
        else:
            party = partydict[n-1]
            for _ in range(1000):
                newparty = list(party)
                oldchars = party
                newchars = [c for c in range(14) if c not in party]
                if n >= 2:
                    oldchars = [c for c in oldchars if c in partydict[n-2]]
                    newchars = [c for c in newchars if c not in partydict[n-2]]
                if (0xE not in party and n < NUM_FLOORS-1
                        and random.randint(1, 20) == 20):
                    newchar = 0xE  # banon
                else:
                    newchar = random.choice(newchars)
                if 0xE in party:
                    oldchar = 0xE
                else:
                    oldchar = random.choice(oldchars)
                newparty.remove(oldchar)
                newparty.append(newchar)
                newparty = tuple(sorted(newparty))
                if newparty not in done_parties:
                    break
            party = newparty
            removedict[n] = oldchar
            addict[n] = newchar
        partydict[n] = party
        if n == 0 and 0 not in party:
            removedict[n] = 0
        if n in removedict:
            fulldict[n] = tuple(sorted(list(partydict[n]) + [removedict[n]]))
        else:
            fulldict[n] = list(partydict[n])
        assert addict[n] in fulldict[n]
        assert len(set(fulldict[n])) == len(fulldict[n])
        assert len(fulldict[n]) in [5, 6]
        done_parties.add(party)

    def write_event(script):
        global fanatix_space_pointer
        if fanatix_space_pointer is None:
            fanatix_space_pointer = addresses.fanatix_space_pointer

        limit = -1
        if fanatix_space_pointer < addresses.fanatix_space_limit:
            assert fanatix_space_pointer >= addresses.fanatix_space_pointer
            limit = addresses.fanatix_space_limit
            if fanatix_space_pointer + len(script) > limit:
                fanatix_space_pointer = addresses.fanatix_space_pointer_2
        if fanatix_space_pointer >= addresses.fanatix_space_limit:
            assert fanatix_space_pointer >= addresses.fanatix_space_pointer_2
            limit = addresses.fanatix_space_limit_2
            if fanatix_space_pointer + len(script) > limit:
                raise Exception('Not enough space.')

        old_pointer = fanatix_space_pointer
        fo.seek(fanatix_space_pointer)
        fo.write(bytes(script))
        fanatix_space_pointer += len(script)
        assert fanatix_space_pointer <= limit
        return old_pointer

    sortuple = lambda x: tuple(sorted(x))
    partial_dict = {}
    for k, v in fulldict.items():
        partial_dict[k] = set(v)

    def get_updated_groupon_dict():
        groupon = []
        for n in range(NUM_FLOORS):
            party = partial_dict[n]
            for i in range(3, 6):
                groupon.extend(map(sortuple, combinations(party, i)))
        groupon_dict = Counter(groupon)
        for k, count in sorted(groupon_dict.items()):
            if (count*len(k)*2) <= (1 + (len(k)*2) + (count*4)):
                del(groupon_dict[k])
        return groupon_dict

    groupon_pointer_dict = {}
    groupon_mapping_dict = defaultdict(set)
    while True:
        groupon_dict = get_updated_groupon_dict()
        if not groupon_dict:
            break
        groupon_score = lambda g: (1 + (len(g)*2) + (groupon_dict[g]*4) -
                                   (groupon_dict[g]*len(g)*2))
        chosen = min(groupon_dict, key=lambda g: groupon_score(g))
        if groupon_score(chosen) >= 0:
            break

        script = []
        for c in chosen:
            script += [0x3D, c]
        script.append(0xFE)
        groupon_pointer = write_event(script)
        groupon_pointer_dict[chosen] = groupon_pointer

        for k, v in partial_dict.items():
            if set(chosen) <= set(v):
                partial_dict[k] = set(v)-set(chosen)
                groupon_mapping_dict[k].add(chosen)

    add_char_scripts = {}
    for i in range(14):
        script = []
        script += [0x3F, i, 0x01]
        if 'BNW' not in get_global_label():
            script += [0x9C, i]  # optimum (glitchy)
        script += [0x3F, i, 0x00]
        script += [0xFE]
        add_char = write_event(script) - 0xA0000
        add_char_scripts[i] = [0xB2] + int_to_bytelist(add_char, 3)

    clear_party_script = []
    clear_party_script += [0x97, 0x5c]  # fade out
    clear_party_script += [
        # IMPORTANT: unequip banon here or lose items permanently
        0x8D, 0x0E,  # yes, do it even if banon's not supposed to be there
        ]
    clear_party_script += [0x46, 0x01]
    for i in range(15):
        clear_party_script += [0x3E, i]
        clear_party_script += [0x3F, i, 0x00]
    clear_party_script += [0xFE]
    clear_party = write_event(clear_party_script) - 0xA0000
    clear_party_command = [0xB2] + int_to_bytelist(clear_party, 3)

    post_boss_script = [
        0xB2] + int_to_bytelist(addresses.gameover_check_pointer-0xA0000,
                                3) + [
        0x3E, 0x10,                         # delete npc
        0x59, 0x08,                         # unfade
        0xFE,
        ]
    post_boss = write_event(post_boss_script) - 0xA0000
    post_boss_command = [0xB2] + int_to_bytelist(post_boss, 3)

    pay_save_script = [
        0xC0, 0xBE, 0x81, 0xFF, 0x69, 0x01,     # check enough money
        0x55, 0x80,                             # flash screen
        0xD2, 0xB5,                             # allow saving
        0xD2, 0xBF,
        0x3A,
        0xFE,
        ]
    pay_save = write_event(pay_save_script) - 0xA0000
    pay_save_command = [0xB2] + int_to_bytelist(pay_save, 3)

    pay_inn_script = [
        0xC0, 0xBE, 0x81, 0xFF, 0x69, 0x01,     # check enough money
        0xB2] + int_to_bytelist(addresses.refreshments_pointer-0xA0000, 3) + [
        0xFE
        ]
    pay_inn = write_event(pay_inn_script) - 0xA0000
    pay_inn_command = [0xB2] + int_to_bytelist(pay_inn, 3)

    done_pay_inns = {}

    LocationObject.class_reseed('prefanatix_espers')
    esper_floors = random.sample(range(NUM_FLOORS), min(27, NUM_FLOORS))
    esper_floors = dict((b, a) for (a, b) in enumerate(esper_floors))

    LocationObject.class_reseed('prefanatix_colosseum')
    valid_floors = [floor for floor in range(NUM_FLOORS)
                    if floor > 0 and floor in addict and addict[floor] <= 13]
    if NUM_FLOORS >= 99:
        colosseum_floors = random.sample(valid_floors, 3)
    elif NUM_FLOORS >= 29:
        colosseum_floors = random.sample(valid_floors, 2)
    else:
        colosseum_floors = [random.choice(valid_floors)]

    LocationObject.class_reseed('prefanatix_bbgs')
    tower_map = LocationObject.get(0x167)
    tower_base = LocationObject.get(0x16a)
    tower_treasure_room = LocationObject.get(0x16d)
    tower_roof = LocationObject.get(0x16c)
    for l in [tower_base, tower_roof]:
        l.set_palette(16)

    candidates = [bbg for bbg in range(0x38) if bbg not in BANNED_BBGS]
    bbgs = []
    while len(bbgs) < NUM_FLOORS-1:
        remaining = (NUM_FLOORS-1) - len(bbgs)
        if remaining >= len(candidates):
            bbgs += candidates
        else:
            bbgs += random.sample(candidates, remaining)
    random.shuffle(bbgs)
    bbgs = [0x2D] + bbgs

    LocationObject.class_reseed('prefanatix_monsters')
    if 'BNW' in get_global_label():
        TRIAD = [0x162, 0x164, 0x163]
        BANNED_FORMATIONS = TRIAD + [
            0x2f, 0xaf, 0x142, 0x167, 0x178, 0x17d, 0x180, 0x1bd, 0x1d7,
            0x200, 0x201, 0x202]
    else:
        TRIAD = [0x1d4, 0x1d6, 0x1d5]  # Doom, Poltrgeist, Goddess
        BANNED_FORMATIONS = TRIAD + [
            #0x3b, 0x3c, 0x3f,
            0x00, 0x22, 0x14f, 0x178,
            0x180, 0x181, 0x182, 0x184, 0x185, 0x186, 0x187, 0x188, 0x189,
            0x1a4, 0x1bd, 0x1c5, 0x1ca, 0x1d7, 0x1d9, 0x1e5, 0x1e8,
            0x1f5, 0x1f8, 0x1fa, 0x1fb, 0x1fc, 0x1fd, 0x1fe, 0x1ff,
            0x200, 0x201, 0x202, 0x20e, 0x232, 0x23e]
    BANNED_FORMATIONS = set(BANNED_FORMATIONS)
    boss_formations = [f for f in FormationObject.ranked
                       if f.two_packs and f.rank > 0
                       and f.index not in BANNED_FORMATIONS]

    duplicates = []
    for f1 in list(boss_formations):
        if f1 not in boss_formations:
            continue
        for f2 in boss_formations[boss_formations.index(f1):]:
            if f1 is f2:
                continue
            if str(sorted(f1.enemies)) == str(sorted(f2.enemies)):
                duplicates.append((f1, f2))
                if f2 in boss_formations:
                    boss_formations.remove(f2)

    unique_boss_formations = sorted(set(boss_formations), key=lambda f: f.rank)
    max_index = len(unique_boss_formations)-1
    while len(boss_formations) < NUM_FLOORS:
        index = random.randint(random.randint(0, max_index), max_index)
        boss_formations.append(unique_boss_formations[index])
    boss_formations = sorted(boss_formations, key=lambda f: f.rank)

    if len(boss_formations) > NUM_FLOORS:
        candidates = random.sample(boss_formations, NUM_FLOORS)
        candidates += random.sample(boss_formations, NUM_FLOORS//2)
        boss_formations = [f for f in boss_formations if f in candidates]
        boss_formations = boss_formations[-NUM_FLOORS:]
    assert len(boss_formations) == NUM_FLOORS

    boss_formations = shuffle_normal(boss_formations,
        wide=True, random_degree=FormationObject.random_degree**2)
    escapable = [bf for bf in boss_formations if not bf.is_inescapable]
    if escapable:
        minboss = min(escapable, key=lambda f: f.rank)
    else:
        minboss = min(boss_formations, key=lambda f: f.rank)
    boss_formations.remove(minboss)
    boss_formations = [minboss] + boss_formations
    assert len(boss_formations) == NUM_FLOORS

    boss_packs = []
    for bf in boss_formations:
        packs = [p for p in TwoPackObject.every if bf in p.formations]
        if len(packs) > 1:
            pack = random.choice(packs)
        else:
            pack = packs[0]
        boss_packs.append(pack)

    packs = [p for p in FourPackObject.ranked
             if p.is_random_encounter and p.rank > 0
             and not set(p.formation_ids) & BANNED_FORMATIONS]
    done_packs = set([])
    chosen_packs = []
    boss_ranks = [bp.rank for bp in boss_packs]
    random_ranks = [p.rank for p in packs]
    minboss, maxboss = min(boss_ranks), max(boss_ranks)
    minrandom, maxrandom = min(random_ranks), max(random_ranks)
    lowratio, highratio = (minrandom / minboss), (maxrandom / maxboss)

    exp_packs = sorted([p for p in packs if p.has_xp], key=lambda x: x.rank)
    chosen = exp_packs[0]
    chosen_packs.append(chosen)
    done_packs.add(chosen)
    highest_threshold = boss_packs[0].rank

    for (i, bp) in enumerate(boss_packs):
        if i == 0:
            continue
        highest_threshold = max(highest_threshold, bp.rank)
        candidates = [p for p in packs if p.rank <= highest_threshold]
        if not candidates:
            candidates = sorted(packs[:2])
        temp = [p for p in candidates if p not in done_packs]
        if temp:
            candidates = temp
        max_index = len(candidates)-1
        floor_ratio = (i / float(NUM_FLOORS-1))**0.5
        ratio = (floor_ratio*highratio) + ((1-floor_ratio)*lowratio)
        threshold = highest_threshold * ratio
        index = len([p for p in candidates if p.rank <= threshold])-1
        assert index <= max_index
        index = max(index, 0)
        index = mutate_normal(index, 0, max_index, wide=True,
                              random_degree=FormationObject.random_degree)
        if i == 0:
            index = 0
        chosen = candidates[index]
        chosen_packs.append(chosen)
        done_packs.add(chosen)

    LocationObject.class_reseed('prefanatix_chests')
    initial_equipment = [i for c in CharacterObject.every[:14]
                         for i in c.old_initial_equipment]
    items = [i for i in ItemObject.ranked if i.rank > 0]
    special_items = [i for i in items if not i.is_buyable
                     and i not in initial_equipment]
    chosen_items = {}
    floors = list(range(NUM_FLOORS))
    random.shuffle(floors)
    for f in floors:
        if random.randint(1, 10) == 10:
            candidates = items
        else:
            candidates = special_items
            if random.randint(1, 10) != 10:
                candidates = [c for c in candidates
                              if c not in chosen_items.values()]
        if not candidates:
            candidates = items
        max_index = len(candidates)-1
        index = (f/float(NUM_FLOORS-1)) * max_index
        index = mutate_normal(index, 0, max_index, wide=True,
                              random_degree=ChestObject.random_degree)
        chosen = candidates[index]
        chosen_items[f] = chosen

    chosen_items = [chosen_items[f] for f in sorted(chosen_items)]
    assert len(chosen_items) == NUM_FLOORS

    LocationObject.class_reseed('prefanatix_music')
    valid_songs = [23, 24, 33, 35, 40, 41, 45, 46, 48, 71, 75, 77, 78, 79]
    avg = lambda stuff: (sum(stuff) / float(len(stuff))) if stuff else 0
    valid_songs = sorted(valid_songs,
        key=lambda s: (avg([l.pack.rank for l in LocationObject.every
                            if l.attacks and l.music == s
                            and l.pack.index > 0]), l.signature, s))
    valid_songs.insert(len(valid_songs)//2, 55)
    valid_songs = shuffle_normal(valid_songs, wide=True)
    if FOOLS and 'BNW' in get_global_label():
        valid_songs = [77]
    chosen_music = {}
    for f in range(NUM_FLOORS):
        ratio = f / float(NUM_FLOORS-1)
        max_index = len(valid_songs)-1
        chosen_music[f] = valid_songs[int(round(max_index*ratio))]

    prev, previous_npc = None, None
    done_shops = set([])
    dummy = ChestObject.create_new()
    dummy.groupindex = 0
    next_map = 0
    while next_map in BANNED_MAPS:
        next_map += 1
    for n in range(NUM_FLOORS):
        # outside section
        LocationObject.get(n).reseed('fanatix')
        l = LocationObject.get(next_map)
        next_map += 1
        while next_map in BANNED_MAPS:
            next_map += 1
        l.purge_associated_objects()
        l.copy_data(tower_map)
        e = EventObject.create_new()
        e.x, e.y = 8, 1
        e.groupindex = prev.index if prev else tower_base.index

        locked = 0
        lockable = [c for c in partydict[n] if c != addict[n]]
        num_locked = (random.randint(0, 1) + random.randint(0, 1)
                      + random.randint(0, 1))
        to_lock = random.sample(lockable, num_locked)
        script = []

        script += clear_party_command
        to_create = list(fulldict[n])
        assert len(to_create) in [5, 6]

        for i in to_create:
            script += [0x3D, i]
        if n in removedict:
            script += [0x8D, removedict[n],]  # remove equips
            locked |= (1 << removedict[n])
            assert removedict[n] not in partydict[n]

        if 0x0E in partydict[n]:
            script += [
                0x88, 0x0E, 0x00, 0x00,     # remove status from banon
                0x40, 0x0E, 0x0E,           # relevel banon
                0x3F, 0x0E, 0x01,
                0x9c, 0x0E,
                ]
            locked |= (1 << 0x0E)
        else:
            if n > 0:
                script += add_char_scripts[addict[n]]
            for i in sorted(to_lock):
                if i == addict[n]:
                    continue
                script += [0x3F, i, 0x01]
                locked |= (1 << i)

            for i in sorted(partydict[n]):
                if i in to_lock or i == addict[n]:
                    continue
                script += [0x3F, i, 0x01]

        for i in range(15):
            if i not in partydict[n]:
                locked |= (1 << i)

        script += [
            0x99, 0x01] + int_to_bytelist(locked, 2) + [        # party select
            0x6B] + int_to_bytelist(l.index | 0x1000, 2) + [9, 27, 0x00,
            ]

        if 0x0E in partydict[n]:
            assert addict[n] == 0xE

        script += [0xFE]
        e.event_addr = write_event(script) - 0xA0000

        npc = NpcObject.create_new()
        npc.groupindex = l.index
        npc.graphics = 0x6F
        npc.set_palette(5)
        npc.facing = 0x43
        npc.x, npc.y = 5, 3
        assert len(l.npcs) == 1
        index = boss_packs[n].index & 0xFF
        script = [
            0x4D, index, 0x3F,   # battle
            ]
        script += post_boss_command
        script += [
            0xD7 | ((next_membit >> 8)*2), next_membit & 0xFF,
            0xFE,
            ]
        npc.set_event_addr(write_event(script) - 0xA0000)
        npc.set_membit(next_membit)
        next_membit += 1

        x = EntranceObject.create_new()
        x.groupindex = l.index
        x.dest = (prev.index if prev else tower_base.index) | 0x3000
        x.x, x.y = 7, 29
        x.destx, x.desty = 7, 2

        # inside section
        l2 = LocationObject.get(next_map)
        next_map += 1
        while next_map in BANNED_MAPS:
            next_map += 1
        l2.purge_associated_objects()
        l2.copy_data(tower_treasure_room)
        l2.set_bit('warpable', False)
        l2.set_bit('enable_encounters', False)
        l2.music = chosen_music[n]
        x = EntranceObject.create_new()
        x.groupindex, x.dest = l.index, l2.index
        if 'JP' not in get_global_label():
            x.dest |= 0x800
        x.x, x.y = 10, 10
        x.destx, x.desty = 7, 12
        x = EntranceObject.create_new()
        x.groupindex, x.dest = l2.index, l.index | 0x2000
        x.x, x.y = 7, 13
        x.destx, x.desty = 10, 11

        c = ChestObject.create_new()
        c.groupindex = l2.index
        c.x, c.y = 7, 6
        c.set_memid(n+1)
        c.set_contents(chosen_items[n])

        ratio = min(n / float(NUM_FLOORS-1), 1.0)
        index = int(round((len(price_message_indexes.keys())-1) * ratio))
        price = sorted(price_message_indexes.keys())[index]
        price_message = price_message_indexes[price]

        if n in esper_floors:
            npc = NpcObject.create_new()
            npc.groupindex = l2.index
            npc.graphics = 0x5B
            npc.facing = 0x54
            npc.set_palette(2)
            npc.x, npc.y = 6, 6
            script = [
                0xF4, 0x8D,
                0x86, esper_floors[n] + 0x36,
                0x3E, 0x10,
                0xFE,
                ]
            event_addr = write_event(script) - 0xA0000
            assert len(l2.npcs) == 1
            npc.set_event_addr(event_addr)

        npc = NpcObject.create_new()
        npc.groupindex = l2.index
        npc.facing = 2
        npc.x, npc.y = 4, 8

        if n == 0:
            npc_choice = 'inn'
        elif n in colosseum_floors:
            npc_choice = 'colosseum'
        else:
            candidates = [
                'save_point', 'save_point', 'inn', 'inn', 'weapon_shop',
                'armor_shop', 'relic_shop', 'item_shop', 'item_shop']
            candidates = [c for c in candidates if c != previous_npc]
            npc_choice = random.choice(candidates)
        previous_npc = npc_choice
        if npc_choice == 'save_point':
            pointer = fanatix_space_pointer - 0xA0000
            npc.become_pay_save(pointer, price, price_message,
                                pay_save_command, write_event)
        elif npc_choice == 'inn':
            npc.graphics = 0x1E
            npc.set_palette(3)
            if price in done_pay_inns:
                npc.set_event_addr(done_pay_inns[price])
            else:
                pointer = fanatix_space_pointer - 0xA0000
                yes_p = pointer + 13
                no_p = yes_p + 7
                script = [
                    0x4B] + int_to_bytelist(price_message, 2) + [   # show $$$
                    0x4B] + int_to_bytelist(addresses.inn_ask_message, 2) + [
                    0xB6] + (int_to_bytelist(yes_p, 3) +
                             int_to_bytelist(no_p, 3)) + [
                    0x85] + int_to_bytelist(price, 2)               # take $$$
                script += pay_inn_command + [0xFE]
                assert script[no_p-pointer:] == [0xFE]
                event_addr = write_event(script) - 0xA0000
                npc.set_event_addr(event_addr)
                done_pay_inns[price] = npc.event_addr
        elif npc_choice == 'colosseum':
            npc.graphics = 0x3B
            npc.set_palette(2)
            npc.set_event_addr(addresses.colosseum_pointer - 0xA0000)
        elif 'shop' in npc_choice:
            if npc_choice == 'weapon_shop':
                npc.graphics = 0x0E
                npc.set_palette(4)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == 'weapons']
            elif npc_choice == 'armor_shop':
                npc.graphics = 0x0E
                npc.set_palette(3)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == 'armor']
            elif npc_choice == 'relic_shop':
                npc.graphics = 0x13
                npc.set_palette(2)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == 'relics']
            else:
                npc.graphics = 0x36
                npc.set_palette(1)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type in ['items', 'misc']]
            shops = sorted(shops, key=lambda s: s.current_rank)
            temp = [s for s in shops if s not in done_shops]
            if temp:
                shops = temp
                max_index = len(shops)-1
                index = int(round(n * max_index / float(NUM_FLOORS-1)))
                index = mutate_normal(index, 0, max_index, wide=True,
                                      random_degree=ShopObject.random_degree)
            else:
                max_index = len(shops)-1
                index = random.randint(0, max_index)
            chosen = shops[index]
            done_shops.add(chosen)
            script = [0x9B, chosen.index,
                      0xFE]
            event_addr = write_event(script) - 0xA0000
            npc.set_event_addr(event_addr)

        npc = NpcObject.create_new()
        npc.groupindex = l2.index
        #npc.groupindex = -1
        npc.graphics = 0x17
        npc.set_palette(0)
        npc.facing = 2
        npc.x, npc.y = 10, 8
        npc.set_event_addr(addresses.unequipper_pointer - 0xA0000)

        l.name_id, l2.name_id = n+1, n+1
        l.set_bit('warpable', False)
        if 'easymodo' in get_activated_codes():
            l.set_bit('enable_encounters', False)
        else:
            l.set_bit('enable_encounters', True)
        l.set_enemy_pack(chosen_packs[n])
        l.set_palette(16)
        l.battlebg = bbgs[n]
        l.music = chosen_music[n]
        prev = l

    # top section
    LocationObject.class_reseed('postfanatix')
    assert next_membit <= 0x100
    x = EntranceObject.create_new()
    x.groupindex = prev.index
    x.x, x.y = 8, 1
    x.dest = tower_roof.index | 0x1000
    x.destx, x.desty = 8, 13

    # Uncomment to create shortcut to top of tower
    #x.groupindex = tower_base.index
    #for e in tower_base.events:
    #    e.groupindex = -1

    x = EntranceObject.create_new()
    x.groupindex = tower_roof.index
    x.x, x.y = 7, 14
    x.dest = prev.index | 0x3000
    x.destx, x.desty = 7, 2

    npc = NpcObject.create_new()
    npc.groupindex = tower_roof.index
    npc.x, npc.y = 4, 5
    pointer = fanatix_space_pointer - 0xA0000
    price = min(price_message_indexes)
    npc.become_pay_save(pointer, price, price_message_indexes[price],
                        pay_save_command, write_event)

    npc = NpcObject.create_new()
    npc.groupindex = tower_roof.index
    npc.graphics = 0x17
    npc.set_palette(0)
    npc.facing = 2
    npc.x, npc.y = 11, 6
    npc.set_event_addr(addresses.unequipper_pointer-0xA0000)

    final_room = LocationObject.get(0x19b)
    for x in final_room.exits:
        x.groupindex = -1
    final_room.set_bit('enable_encounters', False)
    final_room.set_bit('warpable', False)

    tower_base.music = 0x3a
    tower_roof.music = 0x39
    final_room.music = 0

    e = EventObject.create_new()
    e.x, e.y = 7, 6
    e.groupindex = tower_roof.index
    script = list(clear_party_command)
    script += (
        [0xB2] + int_to_bytelist(addresses.load_all_party_pointer-0xA0000, 3))
    locked = 0
    not_locked = list(range(14))
    for i in range(4):
        num_locked = (random.randint(0, 1) + random.randint(0, 1)
                      + random.randint(0, 1))
        for _ in range(num_locked):
            c = random.choice(not_locked)
            locked |= (1 << c)
            script += [0x3F, c, i]
            not_locked.remove(c)
    script += [
        0x46, 0x02,
        0x99, 0x03] + int_to_bytelist(locked, 2) + [    # party select
        0x6B] + int_to_bytelist(final_room.index, 2) + [
            109, 42, 0x00,      # next map
        0xD2, 0xCE,             # enable party switching with Y
        # place party 3 and select it
        0x79, 0x03] + int_to_bytelist(final_room.index, 2) + [
        0x46, 0x03,
        0x45,
        0x31, 0x84, 0xD5, 115, 44, 0xFF,
        0x47,
        0x41, 0x31,
        0x45,
        # place party 1 and select it
        0x79, 0x01] + int_to_bytelist(final_room.index, 2) + [
        0x46, 0x01,
        0x45,
        0x31, 0x84, 0xD5, 103, 45, 0xFF,
        0x47,
        0x41, 0x31,
        0x45,
        0x46, 0x02,
        0x45,
        0x31, 0x84, 0xD5, 109, 42, 0xFF,
        0x47,
        0x45,
        0xFE,
        ]
    e.event_addr = write_event(script) - 0xA0000

    for x, y in [(103, 49), (109, 46), (115, 48)]:
        ex = EntranceObject.create_new()
        ex.groupindex = final_room.index
        ex.dest = tower_roof.index | 0x2000
        ex.x, ex.y = x, y
        ex.destx, ex.desty = 7, 7

    # 19 16 - Flaming house
    # 36 30 - Kefka's background
    final_palette = BBGPaletteObject.get(0x16)
    final_palette.shift_blue()

    script = []
    formation_indexes = list(enumerate(TRIAD))
    random.shuffle(formation_indexes)
    for i, formation_index in formation_indexes:
        tps = [tp for tp in TwoPackObject.every
               if set(tp.formation_ids) == {formation_index}]
        pack = tps[0]
        for f in pack.formations:
            f.clear_music(force=True)
        script += [0xF0, 0x33]  # play fierce battle
        script += [
            0x46, i+1,
            0x4D, pack.index & 0xFF, 0x19 | 0xC0,  # battle no swoosh
            0xB2] + int_to_bytelist(addresses.gameover_check_pointer-0xA0000,
                                    3)

    script += [
        0xF2, 0x80,     # fade out music
        0x95, 0x95,     # pause
        0xDC, 0x7E,     # set/clear bits to fix ending
        0xD7, 0x9F,     # clear bit $39F (1EF3-7)
        0xD7, 0xFF,     # clear bit $3FF (1EFF-7)
        0xB2] + int_to_bytelist(addresses.ending_pointer-0xA0000, 3) + [
        0xFE,
        ]
    fo.seek(addresses.final_pointer)
    fo.write(bytes(script))

    if 'BNW' in get_global_label():
        DialoguePtrObject.bring_back_auction_prices()
        fo.seek(addresses.cheatproof_addr)
        fo.write(bytes(
            [0xB2] + int_to_bytelist(addresses.final_pointer-0xA0000, 3)))

    tower_roof.set_bit('enable_encounters', False)
    tower_roof.set_bit('warpable', False)

    fo.close()


def write_seed():
    if not hasattr(addresses, 'aux_seed_address'):
        return

    address = addresses.aux_seed_address
    length = addresses.aux_seed_length

    degrees = {ao.random_degree for ao in ALL_OBJECTS
               if hasattr(ao, 'random_degree')}
    if degrees == {get_random_degree()}:
        random_degree = int(round((list(degrees)[0]**0.5) * 100))
    else:
        random_degree = '??'
    s = 'BCG v{0} {1} {2}%'.format(VERSION, get_seed(), random_degree)
    s = s[:length-1]
    while len(s) < (length-1):
        s = ' %s ' % s
    s = s[-(length-1):]
    assert 'BNW' in get_global_label()
    s = bnw_encode(s)
    s += b'\x00'
    assert len(s) == length

    f = open(get_outfile(), 'r+b')
    f.seek(address)
    f.write(s)
    f.close()


def handle_exhirom():
    with open(get_outfile(), 'r+b') as f:
        f.seek(0)
        block = f.read(0x10000)
        #f.seek(0x400000)
        #empty = f.read(0x10000)
        #assert empty in [block, b'\x00'*0x10000]
        f.seek(0x400000)
        f.write(block)


def test():
    jm = JunctionManager(get_outfile(), 'bcg_junction_manifest.json')
    jm.add_junction(None, 0x01, 'whitelist')
    jm.add_junction('edgar', 'perfect_taunt', 'blacklist')
    jm.remove_junction('edgar', 0x02, 'blacklist')
    jm.add_junction('plumed hat', 0x02, force_category='equip')
    jm.add_junction('edgar', 'taunt')
    jm.add_junction('bismark', 'pierce_fire')
    jm.add_junction('bismark', 'pierce_ice')
    jm.add_junction('bismark', 'pierce_bolt')
    jm.add_junction('bismark', 'boost_water')
    jm.add_junction('shoat', 'pierce_poison')
    jm.add_junction('edgar', 'initiative')
    jm.add_junction('shadow', 'initiative')
    jm.add_junction('edgar', 'instant_run')
    #jm.add_junction('edgar', 'instant_act')
    jm.add_junction('edgar', 'critical_haste')
    #jm.add_junction('edgar', 'critical_bserk')
    #jm.add_junction('edgar', 'critical_float')
    #jm.add_junction('edgar', 'critical_vanish')
    #jm.add_junction('edgar', 'critical_warp')
    #jm.add_junction('edgar', 'critical_quick')
    #jm.add_junction('edgar', 'critical_cure3')
    #jm.add_junction('edgar', 'critical_remedy')
    #jm.add_junction('edgar', 'critical_regen')
    #jm.add_junction('edgar', 'critical_life3')
    #jm.add_junction('edgar', 'critical_golem')
    #jm.add_junction('edgar', 'critical_fenrir')
    #jm.add_junction('edgar', 'critical_rabbit')
    #jm.add_junction('edgar', 'critical_escape')
    #jm.add_junction('edgar', 'critical_morph')
    #jm.add_junction('edgar', 'critical_runic')
    #jm.add_junction('edgar', 'critical_defend')
    #jm.add_junction('edgar', 'critical_gprain')
    #jm.add_junction('edgar', 'critical_esper')
    #jm.add_junction('edgar', 'critical_jump')
    #jm.add_junction('edgar', 'critical_revenge')
    jm.add_junction('haste', 'salve', 'blacklist')
    jm.add_junction('edgar', 'salve')
    jm.add_junction('edgar', 'repel_fire')
    #jm.add_junction(0x4d, 'initiative', force_category='monster')
    jm.add_junction(0x4d, 'repel_earth', force_category='monster')
    jm.add_junction(0x4d, 'repel_bolt', force_category='monster')
    #jm.add_junction('edgar', 'lucid_dead')
    #jm.add_junction('edgar', 'focus')
    jm.add_junction('gau', 'focus')
    jm.add_junction('umaro', 'focus')
    jm.add_junction('edgar', 'blood_mage')
    jm.add_junction('edgar', 'reflect_boost')
    jm.add_junction('setzer', 'gold_mage')
    #jm.add_junction('setzer', 'null_reflect')
    jm.add_junction('gau', 'poach')
    jm.add_junction('shadow', 'imp_harvest')
    jm.add_junction('leather hat', 'miasma')
    jm.add_junction('leatherarmor', 'miasma')
    jm.add_junction('setzer', 'regenerator')
    jm.add_junction('dragoonboots', 'reverse')
    jm.add_junction('mirage vest', 'heal_boost')
    jm.add_junction(None, 'gilgam_heart')
    jm.add_junction('setzer', 'deaths_door')
    jm.add_junction(None, 'fire_font')
    jm.add_junction(None, 'ice_sink')
    jm.add_junction('edgar', 'necromancer')
    jm.add_junction('edgar', 'commander')
    jm.add_junction('setzer', 'potent_venom')
    jm.add_junction('ifrit', 'boost_esper')
    jm.add_junction('cards', 'boost_nuke')
    #jm.add_junction(None, 'nuke_sink')
    jm.execute()
    return jm


if __name__ == '__main__':
    try:
        print('You are using the Beyond Chaos Gaiden '
              'randomizer version %s.' % VERSION)

        ALL_OBJECTS = [g for g in globals().values()
                       if isinstance(g, type) and issubclass(g, TableObject)
                       and g not in [TableObject]]

        codes = {
            'fanatix': ['fanatix'],
            'wildcommands': ['wildcommands'],
            'easymodo': ['easymodo'],
            'bonanza': ['bonanza'],
        }

        run_interface(ALL_OBJECTS, snes=True, codes=codes,
                      custom_degree=True, custom_difficulty=True)

        tm = gmtime(get_seed())
        if tm.tm_mon == 4 and tm.tm_mday == 1:
            activate_code('fanatix')
            FOOLS = True

        if CmdChangeFBObject.flag in get_flags():
            if get_global_label() in ['FF6_NA_1.0', 'FF6_NA_1.1']:
                write_patch(get_outfile(), 'patch_command_shuffle.txt')
            elif 'FF6_JP' in get_global_label():
                write_patch(get_outfile(), 'patch_command_shuffle_jp.txt')
            elif 'BNW_1' in get_global_label():
                write_patch(get_outfile(), 'patch_command_shuffle_bnw1.txt')
            elif 'BNW_2' in get_global_label():
                write_patch(get_outfile(), 'patch_command_shuffle_bnw2.txt')

        if 'easymodo' in get_activated_codes():
            'EASY MODE ACTIVATED'

        if 'fanatix' in get_activated_codes():
            if get_global_label() in ['FF6_NA_1.0', 'FF6_NA_1.1']:
                write_patch(get_outfile(), 'patch_auto_learn_rage.txt')
            if 'JP' in get_global_label():
                write_patch(get_outfile(), 'patch_let_banon_equip_jp.txt')
                write_patch(get_outfile(), 'patch_auto_learn_rage_jp.txt')
            elif 'SAFE_MODE' not in get_global_label():
                write_patch(get_outfile(), 'patch_let_banon_equip.txt')
            execute_fanatix_mode()

        hexify = lambda x: ' '.join(['{0:0>2x}'.format(c) for c in x])
        numify = lambda x: '{0: >3}'.format(x)
        minmax = lambda x: (min(x), max(x))

        jm = None
        if DEBUG_MODE:
            jm = test()

        write_seed()
        handle_exhirom()

        clean_and_write(ALL_OBJECTS)
        rewrite_snes_meta('BCG-R', VERSION, lorom=False)

        if jm is not None:
            jm.verify_patches()

        for s in SkillObject.every:
            if s.get_bit('ignore_defense'):
                print(s)

        finish_interface()

    except Exception:
        print('ERROR: %s' % format_exc())
        input('Press Enter to close this program. ')
