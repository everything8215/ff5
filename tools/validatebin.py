import os

START_POS = 0x20000
BYTES_TO_CHECK = 0x9FFF

def main():
    with open("../rom/ff5-en.sfc", "rb") as gen_file, open("../rom/Final Fantasy 5.sfc", "rb") as orig_file:
        gen_data = gen_file.read()
        orig_data = orig_file.read()

    # How many addresses you want to see at a time
    top = 5
    # In case you want to skip a few mistakes
    skip_first = 0
    print(f"Skipped first {skip_first} mistakes")

    has_errors = False

    for i in range(START_POS, START_POS + BYTES_TO_CHECK):
        if gen_data[i] != orig_data[i]:
            has_errors = True
            if skip_first > 0:
                skip_first -= 1
                continue
            print(f"Files differ at {i - START_POS:04X}")
            print(f"Gen: {gen_data[i]:02X}, Orig: {orig_data[i]:02X}")
            top -= 1
            if top == 0:
                break
    
    print(f"ROM Matches from 0x{START_POS:06X}-0x{START_POS + BYTES_TO_CHECK:06X}: {not has_errors}")

if __name__ == "__main__":
    main()