import os

bytes_checked = 0
bytes_identical = 0

def check_memory_region(gen_data, orig_data, start_pos, bytes_to_check):
    global bytes_identical
    global bytes_checked
    # How many addresses you want to see at a time
    top = 5
    # In case you want to skip a few mistakes
    skip_first = 0
    if skip_first > 0:
        print(f"Skipped first {skip_first} mistakes")

    has_errors = False

    for i in range(0, bytes_to_check):
        addr = start_pos + i
        if gen_data[addr] != orig_data[addr]:
            has_errors = True
            if skip_first > 0:
                skip_first -= 1
                continue
            print(f"Files differ at {addr:04X}")
            print(f"Gen: {gen_data[addr]:02X}, Orig: {orig_data[addr]:02X}")
            top -= 1
            if top == 0:
                break
        else:
            bytes_identical += 1
    bytes_checked += bytes_to_check
    
    print(f"ROM Matches from 0x{start_pos:06X}-0x{start_pos + bytes_to_check:06X}: {not has_errors}")

def main():
    with open("../rom/ff5-en.sfc", "rb") as gen_file, open("../rom/Final Fantasy 5.sfc", "rb") as orig_file:
        gen_data = gen_file.read()
        orig_data = orig_file.read()

    print("Checking memory regions...\n")

    # C2 Battle Code validation
    check_memory_region(gen_data, orig_data, 0x20000, 0x9FFF)

    # Monster AI validation
    check_memory_region(gen_data, orig_data, 0x109C00, 0x2BFF)

    print(f"\nNumbers of bytes checked: {bytes_checked}")
    print(f"Numbers of bytes identical: {bytes_identical}")
    print(f"% of ROM matched: {(bytes_identical/len(orig_data)*100):10.4f}%")
   

if __name__ == "__main__":
    main()