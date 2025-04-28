from subprocess import run
from sys import argv
from shutil import copyfile

if len(argv) > 3:
    beforefn, afterfn, start, finish = argv[1:]
    start = int(start, 0x10)
    finish = int(finish, 0x10)
else:
    beforefn, afterfn = argv[1:]
    start, finish = None, None

tempfn = '_temp3.smc'
copyfile(beforefn, tempfn)
beforefn = tempfn
with open(beforefn, 'r+b') as f:
    f.seek(0x6fffff)
    peek = f.read(1)
    if not peek:
        peek = b'\x00'
    f.seek(0x6fffff)
    f.write(peek)

cmd = ['cmp', '-l', beforefn, afterfn]
s = run(cmd, capture_output=True).stdout.decode('utf8')

beforeruns = {}
byteruns = {}

for line in s.split('\n'):
    line = line.strip()
    while '  ' in line:
        line = line.replace('  ', ' ')
    if not line:
        continue
    address, before, after = line.split()
    address = int(address)-1
    if start and finish and not (start <= address < finish):
        continue
    before = int(before, 8)
    after = int(after, 8)
    for runaddr, data in sorted(byteruns.items()):
        if runaddr + len(data) == address:
            beforeruns[runaddr] = beforeruns[runaddr] + [before]
            byteruns[runaddr] = data + [after]
            break
    else:
        beforeruns[address] = [before]
        byteruns[address] = [after]

while True:
    sorted_addresses = sorted(byteruns)
    for a, b in zip(sorted_addresses, sorted_addresses[1:]):
        alength = len(byteruns[a])
        assert b > a + alength
        if a < 0x300000:
            hole_width = 8
        else:
            hole_width = 0x40
        if abs(b-(a+alength)) < hole_width:
            blength = len(byteruns[b])
            c = b + blength
            new_length = c - a
            with open(afterfn, 'r+b') as f:
                f.seek(a)
                new_data = [int(x) for x in f.read(new_length)]
            with open(beforefn, 'r+b') as f:
                f.seek(a)
                new_before_data = [int(x) for x in f.read(new_length)]
            byteruns[a] = new_data
            beforeruns[a] = new_before_data
            assert b in byteruns
            assert b in beforeruns
            del(byteruns[b])
            del(beforeruns[b])
            break
    else:
        break

def hexify(data):
    return '-'.join(['{0:0>2x}'.format(c) if c is not None else '  ' for c in data])

def prettify(data, start_address):
    data_length = len(data)
    if len(data) > 0x10:
        left_padding = start_address % 0x10
        data = ([None] * left_padding) + data
    data = hexify(data).split('-')
    formatted = []
    lines = []
    while data:
        line = data[:16]
        data = data[16:]
        words = []
        while line:
            word = ' '.join(line[:4])
            line = line[4:]
            words.append(word)
        line = '  '.join(words)
        lines.append(line)
    last_line = lines[-1]
    end_address = start_address + data_length
    assert len(last_line) <= 58
    last_line = '{0:50}  # {1:0>6x}'.format(last_line, end_address)
    lines = lines[:-1] + [last_line]
    return '\n:       '.join(lines)

for address in sorted(byteruns):
    assert address in beforeruns
    data = byteruns[address]
    print('{0:0>6x}: {1}'.format(address, prettify(data, address)))

print('\nVALIDATION\n')

for address in sorted(beforeruns):
    assert address in byteruns
    data = beforeruns[address]
    print('{0:0>6x}: {1}'.format(address, prettify(data, address)))
