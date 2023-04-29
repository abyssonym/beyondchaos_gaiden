from randomtools.utils import map_to_snes

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
