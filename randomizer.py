from randomtools.tablereader import (
    TableObject, get_global_label, tblpath, addresses, get_random_degree,
    mutate_normal, shuffle_normal, write_patch)
from randomtools.utils import (
    classproperty, cached_property, get_snes_palette_transformer,
    read_multi, write_multi, utilrandom as random)
from randomtools.interface import (
    get_outfile, get_seed, get_flags, get_activated_codes,
    run_interface, rewrite_snes_meta, clean_and_write, finish_interface)
from collections import defaultdict
from os import path
from time import time, sleep
from collections import Counter


VERSION = 0
ALL_OBJECTS = None
DEBUG_MODE = False

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
    60000:  0xa66,
    }


def to_ascii(text):
    s = ""
    for c in text:
        c = ord(c)
        if 0x80 <= c <= 0x99:
            s += chr(ord('A') + c-0x80)
            pass
        elif 0x9A <= c <= 0xB3:
            s += chr(ord('a') + c-0x9A)
        elif 0xB4 <= c <= 0xBD:
            s += chr(ord('0') + c-0xB4)
        else:
            s += '_'
    return s


def int_to_bytelist(value, length):
    value_list = []
    for _ in xrange(length):
        value_list.append(value & 0xFF)
        value >>= 8
    assert value == 0
    return value_list


class InitialMembitObject(TableObject): pass
class CharPaletteObject(TableObject): pass

class CharEsperObject(TableObject):
    @classproperty
    def esper_names(cls):
        return ["Ramuh", "Ifrit", "Shiva", "Siren", "Terrato", "Shoat",
                "Maduin", "Bismark", "Stray", "Palador", "Tritoch", "Odin",
                "Raiden", "Bahamut", "Alexandr", "Crusader", "Ragnarok",
                "Kirin", "ZoneSeek", "Carbunkl", "Phantom", "Sraphim", "Golem",
                "Unicorn", "Fenrir", "Starlet", "Phoenix"]

    def __repr__(self):
        s = ""
        for i in xrange(27):
            if self.allocations & (1 << i):
                s += " " + self.esper_names[i]
        return "%x: %s" % (self.index, s.strip())

    def cleanup(self):
        if "BNW" not in get_global_label():
            self.allocations = 0xFFFFFFFF


class EventObject(TableObject): pass

class NpcObject(TableObject):
    done_pay_saves = {}

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


class InitialRageObject(TableObject):
    def cleanup(self):
        if "fanatix" in get_activated_codes():
            if self.index == 0:
                self.initial_rages = 1
            else:
                self.initial_rages = 0


class SkillObject(TableObject): pass

class ShopObject(TableObject):
    @property
    def items(self):
        return [ItemObject.get(i) for i in self.item_ids if i < 0xFF]

    @property
    def shop_type(self):
        shop_types = {1:"weapons", 2:"armor", 3:"items", 4:"relics", 5:"misc"}
        return shop_types[self.misc & 0x7]

    @property
    def rank(self):
        if set(self.item_ids) == {255}:
            return -1
        return max(i.price for i in self.items)


class MetamorphObject(TableObject):
    def __repr__(self):
        s = "MORPH %x\n" % self.index
        for i in self.items:
            s += str(i.name) + "\n"
        return s.strip()

    @property
    def items(self):
        return [ItemObject.get(i) for i in self.item_ids if i <= 0xFE]


class MagiciteObject(TableObject):
    def cleanup(self):
        assert self.instruction in [0x86, 0x87]
        assert 0x36 <= self.esper_index <= 0x50


class DialoguePtrObject(TableObject):
    @classmethod
    def bring_back_auction_prices(cls):
        if "BNW" not in get_global_label():
            raise NotImplementedError

        indexes = sorted(price_message_indexes.values())
        assert all([i & 0xa00 == 0xa00 for i in indexes])
        pointer = min([DialoguePtrObject.get(i).dialogue_pointer
                       for i in indexes]) | 0xE0000
        message_head = "\x01\x14\x08"
        message_tail = "\x7f\x26\x2f\x5e\x00"
        reverse_dict = dict([(v, k) for (k, v)
                             in price_message_indexes.items()])
        f = open(get_outfile(), "r+b")
        for i in indexes:
            dpo = DialoguePtrObject.get(i)
            dpo.dialogue_pointer = pointer & 0xFFFF
            value = str(reverse_dict[i])
            content = ""
            for c in value:
                content += chr(0x54 + int(c))
            f.seek(pointer)
            s = message_head + content + message_tail
            f.write(s)
            pointer += len(s)


class MonsterObject(TableObject):
    @property
    def name(self):
        return MonsterNameObject.get(self.index).name

    @property
    def ai(self):
        return MonsterAIObject.get(self.index)

    @property
    def true_hp(self):
        return self.hp * self.ai.hp_refills

    @property
    def true_hp_old(self):
        return self.old_data['hp'] * self.ai.hp_refills

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

    @cached_property
    def is_farmable(self):
        for f in FormationObject.every:
            if f.is_random_encounter and self.index in f.old_data['enemy_ids']:
                return True
        return False

    @property
    def rank(self):
        if hasattr(self, "_rank"):
            return self._rank

        MonsterObject.class_reseed("ranking")
        monsters = list(MonsterObject.every)
        monsters = [m for m in monsters if m.old_data['level'] > 0
                    and m.old_data['hp'] < 65535
                    and "Event" not in m.name and set(m.name) != {'_'}]

        score_a = lambda m: (m.old_data['level'], m.true_hp_old,
                             len(m.ai_script), random.random())
        score_b = lambda m: (m.true_hp_old, m.old_data['level'],
                             len(m.ai_script), random.random())
        by_a = sorted(monsters, key=score_a)
        by_b = sorted(monsters, key=score_b)

        for m in MonsterObject.every:
            if m in monsters:
                a, b = (by_a.index(m), by_b.index(m))
                m._rank = max(a, b) * (a+b)
            else:
                m._rank = -1
        return self.rank


class MonsterLootObject(TableObject):
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

class MonsterCtrlObject(TableObject): pass
class MonsterSketchObject(TableObject): pass
class MonsterRageObject(TableObject): pass

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
                args = ""
            script.append(map(ord, value + args))
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
        s = ""
        for line in self.ai_script:
            s += ("%X : " % line[0]) + " ".join(
                ["{0:0>2}".format("%X" % v) for v in line[1:]])
            s = s.strip() + "\n"
        return s.strip()


class PackObject(TableObject):
    def __repr__(self):
        s = "%s-PACK %x:\n%s" % (
            len(self.formations), self.index,
            "\n".join(str(f) for f in self.formations))
        return s

    @property
    def rank(self):
        a = max([f.rank for f in self.formations])
        b = max([f.rank for f in self.formations[:3]])
        return (a+b) / 2.0

    @cached_property
    def old_formation_ids(self):
        formation_ids = []
        for attr in ["common", "common1", "common2", "common3", "rare"]:
            if hasattr(self, attr):
                formation_ids.append(self.old_data[attr])
        assert len(formation_ids) in [2, 4]
        return formation_ids


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
            if not l.get_bit("enable_encounters"):
                continue
            if self.index == l.area_pack.old_data['pack_id']:
                return True

        for zpp in ZonePackPackObject.every:
            if self.index in zpp.old_data['pack_ids']:
                return True

        return False


class TwoPackObject(PackObject):
    @property
    def formations(self):
        return [FormationObject.get(self.common),
                FormationObject.get(self.rare),
                ]


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
        if "fanatix" in get_activated_codes():
            self.encounter_rates = 0


class FormationMetaObject(TableObject):
    @property
    def music(self):
        return (self.music_misc >> 3) & 0x7

    def clear_music(self):
        if self.music == 0:
            self.set_bit("disable_fanfare", True)
            self.set_bit("continue_current_music", True)


class FormationObject(TableObject):
    def __repr__(self):
        s = "FORMATION %x: %s" % (
            self.index, " ".join(e.name for e in self.enemies))
        return s

    @cached_property
    def is_random_encounter(self):
        for p in FourPackObject.every:
            if (p.is_random_encounter
                    and self.index in p.old_formation_ids):
                return True
        return False

    @cached_property
    def is_inescapable(self):
        for e in self.enemies:
            if e.get_bit("is_inescapable"):
                return True

    @property
    def metadata(self):
        return FormationMetaObject.get(self.index)

    def clear_music(self):
        self.metadata.clear_music()

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
        enemy_ranks = [e.rank for e in self.enemies if e.rank > 0]
        if not enemy_ranks:
            return -1
        return max(enemy_ranks) * (sum(enemy_ranks)**0.0625)

    @cached_property
    def two_packs(self):
        return [p for p in TwoPackObject.every if self in p.formations]


class MonsterNameObject(TableObject):
    @property
    def name(self):
        return to_ascii(self.name_text)

    @classmethod
    def full_cleanup(cls):
        if hasattr(addresses, "sort_rages_address"):
            f = open(get_outfile(), 'r+b')
            counter = 0
            for mno in sorted(MonsterNameObject.every, key=lambda n: n.name):
                if mno.index >= 0x100:
                    continue
                f.seek(addresses.sort_rages_address + counter)
                f.write(chr(mno.index))
                if hasattr(addresses, "myself_rages_address"):
                    f.seek(addresses.myself_rages_address + counter)
                    f.write(chr(mno.index))
                counter += 1
            assert counter <= 0x100
            f.close()

        super(MonsterNameObject, cls).full_cleanup()


class ItemNameObject(TableObject):
    @property
    def name(self):
        return to_ascii(self.name_text)

class ItemObject(TableObject):
    @property
    def name(self):
        return ItemNameObject.get(self.index).name

    @property
    def is_legit(self):
        if "BNW" in get_global_label():
            return set(self.name) != {'_'}
        return True

    @property
    def pretty_type(self):
        return {0: "tool",
                1: "weapon",
                2: "armor",
                3: "shield",
                4: "helm",
                5: "relic",
                6: "consumable"}[self.itemtype & 0x7]

    @property
    def rank(self):
        if hasattr(self, "_rank"):
            return self._rank

        ItemObject.class_reseed("ranking")

        if "BNW" in get_global_label():
            BANNED_INDEXES = [0x10]  # Narpas
        else:
            BANNED_INDEXES = []

        # TODO: also consider morphs?
        tier0 = [i for i in ItemObject.every if i.is_buyable]
        tier0 = sorted(tier0, key=lambda i: (i.is_buyable, random.random()))
        tier1 = [i for i in ItemObject.every
                 if i.is_farmable and i not in tier0]
        tier1 = sorted(tier1, key=lambda i: (i.is_farmable, random.random()))
        tier2b = [i for i in ItemObject.every
                  if i.is_boss_loot and i not in tier0 + tier1]
        tier2b = sorted(tier2b,
                        key=lambda i: (i.is_boss_loot, random.random()))
        tier2c = [i for i in ItemObject.every
                  if i.is_chest and i not in tier0 + tier1]
        tier2c = sorted(tier2c, key=lambda i: (i.is_chest, random.random()),
                        reverse=True)
        tier2 = [i for i in ItemObject.every
                 if i in tier2b and i in tier2c]
        tier2 = sorted(
            tier2, key=lambda i: min(
                tier2b.index(i), tier2c.index(i), random.random()))
        tier3 = [i for i in ItemObject.every
                 if i in tier2b + tier2c and i not in tier2]

        def t3_sorter(i):
            mylist = tier2b if i in tier2b else tier2c
            return (mylist.index(i) / float(len(mylist)-1), random.random())

        tier3 = sorted(tier3, key=t3_sorter)
        tier4 = [i for i in ItemObject.every if i.is_legit and
                 i not in tier0 + tier1 + tier2 + tier3]
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
                if 0 < colosseum_rank + 0.1 < i._rank_no_colosseum:
                    i._rank = colosseum_rank + 0.1

        full_list = sorted(full_list, key=lambda i: (i._rank, random.random()))
        for i in full_list:
            i._rank = full_list.index(i)

        for i in ItemObject.every:
            if i.index in BANNED_INDEXES or not hasattr(i, "_rank"):
                i._rank = -1

        return self.rank

    @cached_property
    def is_buyable(self):
        for s in ShopObject.every:
            if self.index in s.old_data['item_ids']:
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
                cs.append(c)
        return len(cs)

    @property
    def is_colosseum(self):
        if hasattr(self, "_is_colosseum"):
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

    def cleanup(self):
        if "fanatix" in get_activated_codes():
            if self.index in [0xF6, 0xF7]:
                self.price = 0
                self.otherproperties = 0
                self.itemtype = 6


class EsperObject(TableObject):
    def __repr__(self):
        s = "ESPER %x\n" % self.index
        for i in xrange(1, 6):
            s += "{0:0>2}".format("%x" % getattr(self, "spell%s" % i))
            s += " x%s\n" % getattr(self, "learn%s" % i)
        return s.strip()


class ColosseumObject(TableObject):
    def __repr__(self):
        return "%s -> %s : %s" % (
            self.item.name, self.trade.name, self.opponent.name)

    @property
    def is_legit(self):
        if "JP" not in get_global_label():
            return "chupon" not in str(self).lower()
        raise NotImplementedError

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

class ExperienceObject(TableObject):
    def cleanup(self):
        if "fanatix" in get_activated_codes():
            self.experience /= 2

class LocNamePtrObject(TableObject): pass

class ChestObject(TableObject):
    @property
    def memid(self):
        memid = self.memid_low
        if self.get_bit("memid_high"):
            memid |= 0x100
        return memid

    def set_memid(self, index):
        assert index <= 0x1FF
        if self.index & 0x100:
            self.set_bit("memid_high", True)
        else:
            self.set_bit("memid_high", False)
        self.memid_low = index & 0xFF

    @cached_property
    def old_treasure(self):
        if self.old_data['misc'] & 0x40:
            return ItemObject.get(self.old_data['contents'])
        return None

    def set_contents(self, item):
        if isinstance(item, ItemObject):
            self.set_bit("treasure", True)
            item = item.index
        elif isinstance(item, FormationObject):
            raise NotImplementedError
        self.contents = item


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


class LongEntranceObject(TableObject): pass

class CharacterObject(TableObject):
    @property
    def old_initial_equipment_ids(self):
        return ([self.old_data[attr]
                 for attr in ["weapon", "shield", "helm", "armor"]]
                + self.old_data["relics"])

    @property
    def old_initial_equipment(self):
        return [ItemObject.get(i)
                for i in self.old_initial_equipment_ids if i <= 0xFE]

    def cleanup(self):
        if "fanatix" in get_activated_codes():
            self.level &= 0xF3
            if self.index == 0x0E:
                self.level |= 0x08
                self.relics = [0xFF, 0xFF]
                for attr in ["weapon", "shield", "helm", "armor"]:
                    '''
                    # make banon's default equipment optimum-friendly
                    candidates = set([
                        getattr(c, attr) for c in CharacterObject.every[:12]
                        if getattr(c, attr) < 0xFF])
                    items = sorted([ItemObject.get(c) for c in candidates],
                                    key=lambda i: i.price)
                    items = [i for i in items if i.pretty_type == attr]
                    chosen = items[0]
                    chosen.price = 1
                    setattr(self, attr, chosen.index)
                    '''
                    setattr(self, attr, 0xFF)


def number_location_names():
    pointer = addresses.location_names
    f = open(get_outfile(), 'r+b')
    f.seek(pointer)
    f.write('\x00')
    for i in xrange(1, 101):
        pointer = f.tell() - addresses.location_names
        LocNamePtrObject.get(i).name_pointer = pointer
        s = "{0:0>2}".format(i)
        for c in s:
            v = int(c)
            f.write(chr(0x54 + v))
        f.write('\x00')
    assert f.tell() <= addresses.location_names_max
    f.close()


fanatix_space_pointer = None


def execute_fanatix_mode():
    print "FANATIX MODE ACTIVATED"

    for i in xrange(32):
        InitialMembitObject.get(i).membyte = 0xFF

    BANNED_BBGS = [
        0x07, 0x0D, 0x25, 0x29, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36]
    BANNED_MAPS = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x07, 0x0B, 0x0C, 0x0D,
        0x11, 0x14, 0x15, 0x22, 0x2f, 0x37, 0x40, 0x4b, 0x50, 0x53, 0x5b,
        0x60, 0x75, 0x7b, 0x7d, 0x7e, 0x7f,
        0x81, 0x82, 0x88, 0x89, 0x8c, 0x8f,
        0x90, 0x92, 0x99, 0x9c, 0x9d, 0xa9,
        0xb6, 0xb7, 0xb8, 0xbd, 0xbe,
        0xcd, 0xcf, 0xd0, 0xd1, 0xd9, 0xdd,
        0xd2, 0xd3, 0xd4, 0xd5, 0xd7,
        0xe1, 0xe7, 0xe9, 0xea, 0xeb,
        0xfd, 0xfe, 0xff,
        0x100, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x10c, 0x12e,
        0x131, 0x132, 0x139, 0x13a, 0x13b, 0x13c, 0x13d, 0x13e,
        0x141, 0x142, 0x143, 0x144,
        0x150, 0x154, 0x155, 0x157, 0x158,
        ]
    BANNED_MAPS += range(0x160, 0x200)
    BANNED_MAPS.remove(0x16a)
    BANNED_MAPS.remove(0x16c)

    for l in LocationObject.every:
        if l.index in BANNED_MAPS:
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

    number_location_names()

    opening_event = [
        0xB8, 0x42,                         # enable morph
        0xB8, 0x43,                         # show magic points after battle
        0xB8, 0x4B,                         # shadow can't leave
        0x88, 0x00, 0x00, 0x00,             # remove magitek status from terra
        0x3F, 0x00, 0x00,                   # remove terra
        0x3E, 0x00,
        0x3F, 0x0E, 0x00,                   # remove biggs
        0x3E, 0x0E,
        0x3F, 0x0F, 0x00,                   # remove wedge
        0x3E, 0x0F,
        ]
    for i in xrange(0xE):
        opening_event += [
            0x7F, i, i,     # character name
            0x37, i, i,     # character sprite
            0x43, i, CharPaletteObject.get(i).palette_index,
            0x40, i, i,     # character data
            0xD4, 0xE0+i,
            0xD4, 0xF0+i,
            ]

    #for i in xrange(27):   # espers
    #    opening_event += [0x86, i + 0x36,]

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
    fo.write("".join(map(chr, [0xFD]*4)))  # no opening crawl
    opening_jump_pointer = addresses.opening_jump_pointer
    fo.seek(addresses.opening_pointer)
    fo.write("".join(map(chr,
        [0xB2] + int_to_bytelist(opening_jump_pointer-0xA0000, 3) + [0xFE])))
    fo.seek(opening_jump_pointer)
    fo.write("".join(map(chr, opening_event)))

    partydict = {}
    removedict, addict = {}, {}
    done_parties = set([])
    NUM_FLOORS = 99
    #NUM_FLOORS = 49
    #NUM_FLOORS = 2
    next_membit = 1
    LocationObject.class_reseed("prefanatix")
    for n in xrange(NUM_FLOORS):
        if n == 0:
            party = tuple(sorted(random.sample(range(14),5)))
            addict[n] = random.choice(party)
        else:
            party = partydict[n-1]
            for _ in xrange(1000):
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
        done_parties.add(party)

    limit = addresses.fanatix_space_limit
    def write_event(script):
        global fanatix_space_pointer
        if fanatix_space_pointer is None:
            fanatix_space_pointer = addresses.fanatix_space_pointer
        old_pointer = fanatix_space_pointer
        fo.seek(fanatix_space_pointer)
        fo.write("".join(map(chr, script)))
        fanatix_space_pointer += len(script)
        assert fanatix_space_pointer <= limit
        return old_pointer

    clear_party_script = []
    clear_party_script += [
        # IMPORTANT: unequip banon here or lose items permanently
        0x8D, 0x0E,
        ]
    clear_party_script += [0x46, 0x01]
    for i in xrange(15):
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

    LocationObject.class_reseed("prefanatix_espers")
    esper_floors = random.sample(range(NUM_FLOORS), min(27, NUM_FLOORS))
    esper_floors = dict((b, a) for (a, b) in enumerate(esper_floors))

    LocationObject.class_reseed("prefanatix_colosseum")
    if NUM_FLOORS >= 99:
        colosseum_floors = random.sample(range(NUM_FLOORS), 3)
    elif NUM_FLOORS >= 29:
        colosseum_floors = random.sample(range(NUM_FLOORS), 2)
    else:
        colosseum_floors = [random.randint(0, NUM_FLOORS-1)]

    LocationObject.class_reseed("prefanatix_bbgs")
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

    LocationObject.class_reseed("prefanatix_monsters")
    if "BNW" in get_global_label():
        TRIAD = [0x162, 0x164, 0x163]
        BANNED_FORMATIONS = TRIAD + [0x142, 0x1bd, 0x1d7, 0x200, 0x201, 0x202]
    else:
        TRIAD = [0x1d4, 0x1d6, 0x1d5]  # Doom, Poltrgeist, Goddess
        BANNED_FORMATIONS = TRIAD + [
            0x187, 0x1a4, 0x1bd, 0x1c5, 0x1ca, 0x1d7, 0x1e5, 0x1fa,
            0x200, 0x201, 0x202, 0x20e, 0x232]
    done_monsters = set([])
    formations = [f for f in FormationObject.every
                  if f.two_packs and f.rank > 0
                  and f.index not in BANNED_FORMATIONS]
    boss_formations = [f for f in formations if not f.is_random_encounter]
    extra_formations = [f for f in formations if f not in boss_formations]
    extra_formations += [f for f in formations
                         if f.is_random_encounter and f.is_inescapable
                         and f not in boss_formations + extra_formations]
    new_formations = []
    random.shuffle(boss_formations)
    for f in boss_formations:
        if set(f.enemies) <= done_monsters:
            continue
        new_formations.append(f)
        done_monsters |= set(f.enemies)
    extra_formations += [f for f in boss_formations if f not in new_formations]
    assert not set(extra_formations) & set(new_formations)
    while len(new_formations) < NUM_FLOORS:
        if extra_formations:
            f = random.choice(extra_formations)
            new_formations.append(f)
        else:
            new_formations.append(random.choice(new_formations))
    if len(new_formations) > NUM_FLOORS:
        new_formations = random.sample(new_formations, NUM_FLOORS)
    all_formations = [f for f in FormationObject.every
                      if f in boss_formations + new_formations]
    all_formations = sorted(all_formations)
    all_formations = shuffle_normal(
        all_formations, random_degree=FormationObject.random_degree**1.5)
    boss_formations = sorted(
            new_formations, key=lambda f: all_formations.index(f))
    assert len(boss_formations) == NUM_FLOORS

    boss_packs = []
    for bf in boss_formations:
        packs = [p for p in TwoPackObject.every if bf in p.formations]
        if len(packs) > 1:
            pack = random.choice(packs)
        else:
            pack = packs[0]
        boss_packs.append(pack)

    packs = [p for p in FourPackObject.every
             if p.is_random_encounter and p.rank > 0]
    done_packs = set([])
    chosen_packs = []
    highest_threshold = 0
    for (i, bp) in enumerate(boss_packs):
        highest_threshold = max(highest_threshold, bp.rank)
        candidates = [p for p in packs if p.rank <= highest_threshold]
        if not candidates:
            candidates = sorted(packs[:2])
        temp = [p for p in candidates if p not in done_packs]
        if temp:
            candidates = temp
        max_index = len(candidates)-1
        index = int(round(max_index * ((i/float(NUM_FLOORS))**0.5)))
        index = mutate_normal(index, 0, max_index, wide=True,
                              random_degree=FormationObject.random_degree)
        chosen = candidates[index]
        chosen_packs.append(chosen)
        done_packs.add(chosen)
    for pack in sorted(done_packs):
        for f in pack.formations:
            f.clear_music()

    LocationObject.class_reseed("prefanatix_chests")
    initial_equipment = [i for c in CharacterObject.every[:14]
                         for i in c.old_initial_equipment]
    items = [i for i in ItemObject.ranked if i.rank > 0]
    special_items = [i for i in items if not i.is_buyable
                     and i not in initial_equipment]
    chosen_items = {}
    floors = range(NUM_FLOORS)
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

    prev = None
    dummy = ChestObject.create_new()
    dummy.groupindex = 0
    next_map = 0
    while next_map in BANNED_MAPS:
        next_map += 1
    for n in xrange(NUM_FLOORS):
        # outside section
        LocationObject.get(n).reseed("fanatix")
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
        for i in partydict[n]:
            script += [0x3D, i]
        if n in removedict:
            script += [0x3D, removedict[n]]
            locked |= (1 << removedict[n])
            assert removedict[n] not in partydict[n]
            if removedict[n] == 0x0E:
                script += [0x8D, 0x0E]

        if 0x0E in partydict[n]:
            script += [
                0x88, 0x0E, 0x00, 0x00,     # remove status from banon
                0x40, 0x0E, 0x0E,           # relevel banon
                0x3F, 0x0E, 0x01,
                #0x9C, 0x0E,                 # optimum (glitchy)
                ]
            locked |= (1 << 0x0E)
        else:
            for i in sorted(to_lock):
                if i == addict[n]:
                    continue
                script += [0x3F, i, 0x01]
                locked |= (1 << i)

            for i in sorted(partydict[n]):
                if i in to_lock or i == addict[n]:
                    continue
                script += [0x3F, i, 0x01]

        for i in xrange(15):
            if i not in partydict[n]:
                locked |= (1 << i)

        script += [
            0x99, 0x01] + int_to_bytelist(locked, 2) + [        # party select
            0x6B] + int_to_bytelist(l.index | 0x1000, 2) + [9, 27, 0x00,
            ]

        if 0x0E in partydict[n] and "BNW" not in get_global_label():
            script += [0x9C, 0x0E]  # optimum (glitchy)

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
        #l2.set_bit("warpable", True)
        l2.set_bit("enable_encounters", False)
        x = EntranceObject.create_new()
        x.groupindex, x.dest = l.index, l2.index | 0x800
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

        if n in colosseum_floors:
            npc_choice = "colosseum"
        else:
            npc_choice = random.choice(["save_point", "inn", "weapon_shop",
                                        "armor_shop", "relic_shop",
                                        "item_shop", "item_shop"])
        if npc_choice == "save_point":
            pointer = fanatix_space_pointer - 0xA0000
            npc.become_pay_save(pointer, price, price_message,
                                pay_save_command, write_event)
        elif npc_choice == "inn":
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
        elif npc_choice == "colosseum":
            npc.graphics = 0x3B
            npc.set_palette(2)
            npc.set_event_addr(addresses.colosseum_pointer - 0xA0000)
        elif "shop" in npc_choice:
            if npc_choice == "weapon_shop":
                npc.graphics = 0x0E
                npc.set_palette(4)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == "weapons"]
            elif npc_choice == "armor_shop":
                npc.graphics = 0x0E
                npc.set_palette(3)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == "armor"]
            elif npc_choice == "relic_shop":
                npc.graphics = 0x13
                npc.set_palette(0)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type == "relics"]
            else:
                npc.graphics = 0x36
                npc.set_palette(1)
                shops = [s for s in ShopObject.every
                         if s.rank > 0 and s.shop_type in ["items", "misc"]]
            chosen = random.choice(shops)
            script = [0x9B, chosen.index,
                      0xFE]
            event_addr = write_event(script) - 0xA0000
            npc.set_event_addr(event_addr)

        npc = NpcObject.create_new()
        npc.groupindex = l2.index
        npc.graphics = 0x17
        npc.set_palette(0)
        npc.facing = 2
        npc.x, npc.y = 10, 8
        npc.set_event_addr(addresses.unequipper_pointer - 0xA0000)

        l.name_id, l2.name_id = n+1, n+1
        l.set_bit("enable_encounters", True)
        l.set_enemy_pack(chosen_packs[n])
        l.set_palette(16)
        l.battlebg = bbgs[n]
        prev = l

    # top section
    LocationObject.class_reseed("postfanatix")
    assert next_membit <= 0x100
    x = EntranceObject.create_new()
    x.groupindex = prev.index
    x.x, x.y = 8, 1
    x.dest = tower_roof.index | 0x1000
    x.destx, x.desty = 8, 13

    x = EntranceObject.create_new()
    x.groupindex = tower_roof.index
    x.x, x.y = 7, 14
    x.dest = prev.index | 0x3000
    x.destx, x.desty = 7, 2

    npc = NpcObject.create_new()
    npc.groupindex = tower_roof.index
    npc.x, npc.y = 4, 5
    pointer = fanatix_space_pointer - 0xA0000
    npc.become_pay_save(pointer, min(price_message_indexes), price_message,
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

    e = EventObject.create_new()
    e.x, e.y = 7, 6
    e.groupindex = tower_roof.index
    script = list(clear_party_command)
    script += (
        [0xB2] + int_to_bytelist(addresses.load_all_party_pointer-0xA0000, 3))
    locked = 0
    not_locked = range(14)
    for i in xrange(4):
        num_locked = (random.randint(0, 1) + random.randint(0, 1)
                      + random.randint(0, 1))
        for _ in xrange(num_locked):
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

    script = []
    pack_indexes = list(enumerate(TRIAD))
    random.shuffle(pack_indexes)
    for i, pack_index in pack_indexes:
        script += [
            0x46, i+1,
            0x4D, pack_index & 0xFF, 0x36,
            0xB2] + int_to_bytelist(addresses.gameover_check_pointer-0xA0000,
                                    3)

    script += [
        0xDC, 0x7E,     # set/clear bits to fix ending
        0xD7, 0x9F,     # clear bit $39F (1EF3-7)
        0xD7, 0xFF,     # clear bit $3FF (1EFF-7)
        0xB2] + int_to_bytelist(addresses.ending_pointer-0xA0000, 3) + [
        0xFE,
        ]
    fo.seek(addresses.final_pointer)
    fo.write("".join(map(chr, script)))

    if "BNW" in get_global_label():
        DialoguePtrObject.bring_back_auction_prices()
        fo.seek(addresses.cheatproof_addr)
        fo.write("".join(map(chr,
            [0xB2] + int_to_bytelist(addresses.final_pointer-0xA0000, 3))))


    tower_roof.set_bit("enable_encounters", False)

    fo.close()


if __name__ == "__main__":
    try:
        print ("You are using the Beyond Chaos Gaiden "
               "randomizer version %s." % VERSION)
        print

        ALL_OBJECTS = [g for g in globals().values()
                       if isinstance(g, type) and issubclass(g, TableObject)
                       and g not in [TableObject]]

        codes = {
            "fanatix": ["fanatix"],
        }
        run_interface(ALL_OBJECTS, snes=True, codes=codes)

        if "fanatix" in get_activated_codes():
            if get_global_label() in ["FF6_NA_1.0", "FF6_NA_1.1"]:
                write_patch(get_outfile(), "auto_learn_rage_patch.txt")
            write_patch(get_outfile(), "let_banon_equip_patch.txt")
            execute_fanatix_mode()

        hexify = lambda x: "{0:0>2}".format("%x" % x)
        numify = lambda x: "{0: >3}".format(x)
        minmax = lambda x: (min(x), max(x))

        clean_and_write(ALL_OBJECTS)
        rewrite_snes_meta("BCG-R", VERSION, lorom=False)

        finish_interface()

    except Exception, e:
        print "ERROR: %s" % e
        raw_input("Press Enter to close this program.")
