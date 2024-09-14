import json
import csv

### Load CSV Classifier Data into a List of Dictionaries
def load_csv_ranges(csv_path):
    ranges = []
    with open(csv_path, 'r') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            # Convert hex string to integers
            address_start = int(row['address start'], 16)
            address_end = int(row['address end'], 16)
            type_info = row['type']
            ranges.append({
                'start': address_start,
                'end': address_end,
                'type': type_info
            })
    return ranges

### Semi-complete opcode dictionary for the 65816 architecture
opcode_dict_65816 = {
    0x00: "BRK",
    0x01: "ORA (dp,X)",
    0x05: "ORA dp",
    0x06: "ASL dp",
    0x08: "PHP",
    0x09: "ORA #imm",
    0x0A: "ASL A",
    0x0D: "ORA abs",
    0x0E: "ASL abs",
    0x10: "BPL",
    0x11: "ORA (dp),Y",
    0x15: "ORA dp,X",
    0x16: "ASL dp,X",
    0x18: "CLC",
    0x19: "ORA abs,Y",
    0x1D: "ORA abs,X",
    0x1E: "ASL abs,X",
    0x20: "JSR abs",
    0x21: "AND (dp,X)",
    0x22: "JSL",
    0x24: "BIT dp",
    0x25: "AND dp",
    0x26: "ROL dp",
    0x28: "PLP",
    0x29: "AND #imm",
    0x2A: "ROL A",
    0x2C: "BIT abs",
    0x2D: "AND abs",
    0x2E: "ROL abs",
    0x30: "BMI",
    0x31: "AND (dp),Y",
    0x35: "AND dp,X",
    0x36: "ROL dp,X",
    0x38: "SEC",
    0x39: "AND abs,Y",
    0x3D: "AND abs,X",
    0x3E: "ROL abs,X",
    0x40: "RTI",
    0x41: "EOR (dp,X)",
    0x45: "EOR dp",
    0x46: "LSR dp",
    0x48: "PHA",
    0x49: "EOR #imm",
    0x4A: "LSR A",
    0x4C: "JMP abs",
    0x4D: "EOR abs",
    0x4E: "LSR abs",
    0x50: "BVC",
    0x51: "EOR (dp),Y",
    0x55: "EOR dp,X",
    0x56: "LSR dp,X",
    0x58: "CLI",
    0x59: "EOR abs,Y",
    0x5A: "PHY",
    0x5D: "EOR abs,X",
    0x5E: "LSR abs,X",
    0x60: "RTS",
    0x61: "ADC (dp,X)",
    0x65: "ADC dp",
    0x66: "ROR dp",
    0x68: "PLA",
    0x69: "ADC #imm",
    0x6A: "ROR A",
    0x6B: "RTL",
    0x6C: "JMP (abs)",
    0x6D: "ADC abs",
    0x6E: "ROR abs",
    0x70: "BVS",
    0x71: "ADC (dp),Y",
    0x75: "ADC dp,X",
    0x76: "ROR dp,X",
    0x78: "SEI",
    0x79: "ADC abs,Y",
    0x7A: "PLY",
    0x7D: "ADC abs,X",
    0x7E: "ROR abs,X",
    0x80: "BRA",
    0x81: "STA (dp,X)",
    0x84: "STY dp",
    0x85: "STA dp",
    0x86: "STX dp",
    0x88: "DEY",
    0x8A: "TXA",
    0x8C: "STY abs",
    0x8D: "STA abs",
    0x8E: "STX abs",
    0x90: "BCC",
    0x91: "STA (dp),Y",
    0x94: "STY dp,X",
    0x95: "STA dp,X",
    0x96: "STX dp,Y",
    0x98: "TYA",
    0x99: "STA abs,Y",
    0x9A: "TXS",
    0x9D: "STA abs,X",
    0xA0: "LDY #imm",
    0xA1: "LDA (dp,X)",
    0xA2: "LDX #imm",
    0xA4: "LDY dp",
    0xA5: "LDA dp",
    0xA6: "LDX dp",
    0xA8: "TAY",
    0xA9: "LDA #imm",
    0xAA: "TAX",
    0xAC: "LDY abs",
    0xAD: "LDA abs",
    0xAE: "LDX abs",
    0xB0: "BCS",
    0xB1: "LDA (dp),Y",
    0xB4: "LDY dp,X",
    0xB5: "LDA dp,X",
    0xB6: "LDX dp,Y",
    0xB8: "CLV",
    0xB9: "LDA abs,Y",
    0xBA: "TSX",
    0xBC: "LDY abs,X",
    0xBD: "LDA abs,X",
    0xBE: "LDX abs,Y",
    0xC0: "CPY #imm",
    0xC1: "CMP (dp,X)",
    0xC2: "REP",
    0xC4: "CPY dp",
    0xC5: "CMP dp",
    0xC6: "DEC dp",
    0xC8: "INY",
    0xC9: "CMP #imm",
    0xCA: "DEX",
    0xCC: "CPY abs",
    0xCD: "CMP abs",
    0xCE: "DEC abs",
    0xD0: "BNE",
    0xD1: "CMP (dp),Y",
    0xD5: "CMP dp,X",
    0xD6: "DEC dp,X",
    0xD8: "CLD",
    0xD9: "CMP abs,Y",
    0xDA: "PHX",
    0xDD: "CMP abs,X",
    0xDE: "DEC abs,X",
    0xE0: "CPX #imm",
    0xE1: "SBC (dp,X)",
    0xE2: "SEP",
    0xE4: "CPX dp",
    0xE5: "SBC dp",
    0xE6: "INC dp",
    0xE8: "INX",
    0xE9: "SBC #imm",
    0xEA: "NOP",
    0xEB: "XBA",
    0xEC: "CPX abs",
    0xED: "SBC abs",
    0xEE: "INC abs",
    0xF0: "BEQ",
    0xF1: "SBC (dp),Y",
    0xF5: "SBC dp,X",
    0xF6: "INC dp,X",
    0xF8: "SED",
    0xF9: "SBC abs,Y",
    0xFA: "PLX",
    0xFD: "SBC abs,X",
    0xFE: "INC abs,X"
}

### Get the Type of a Given Address
def get_address_type(address, ranges):
    for range_item in ranges:
        if range_item['start'] <= address <= range_item['end']:
            return range_item['type']
    return 'Unknown'

### Detect End of ASM Function
def is_function_end(opcode):
    return opcode in [0x10, 0x30, 0x40, 0x4C, 0x60, 0x6B, 0x80, 0xD0, 0xF0] 
# Above are: BPL, BMI, RTI, JMP, RTS, RTL, BRA, BNE, BEQ

### Parsing Logic: Now with Byte Classification from CSV Data!
def parse_opcodes(file_bytes, ranges_list):
    functions = []
    current_function = None
    current_data_block = None  # Track the current data block
    current_other_block = None  # Track the current other block

    for address, byte in enumerate(file_bytes):
        # Convert byte to its hex representation
        opcode = f"{int(byte, 16):02X}" if isinstance(byte, str) else f"{byte:02X}"
        address_str = f"{address:06X}"

        # Determine type based on CSV ranges
        byte_type = get_address_type(address, ranges_list)
        
        if byte_type == "ASM":
            # Ensure opcode is an integer
            if isinstance(opcode, str):
                opcode = int(opcode, 16)

            # Fetch the opcode's instruction designator from the dictionary
            opcode_instruction = opcode_dict_65816.get(opcode, None)

            # Create the instruction string with the designator and hex value
            if opcode_instruction:
                instruction = f"{opcode_instruction} ({opcode:02X})" # hex formatted value
            else:
                instruction = f"Unknown opcode ({opcode:02X})"

            # Convert opcode to string if it's not already, and handle it for the function end check
            opcode_hex = f"{opcode:02X}" if isinstance(opcode, int) else opcode

            # Check if the instruction is a function end
            if is_function_end(int(opcode_hex, 16)):
                # Append the final instruction to the current function
                if current_function is not None:
                    current_function["instructions"].append(f"{address_str}: {instruction}")
                    functions.append(current_function)
                current_function = None
            else:
                # Regular ASM opcode
                if current_function is None:
                    current_function = {
                        "name": f"Function_{address_str}",
                        "type": "ASM",
                        "instructions": []
                    }
                current_function["instructions"].append(f"{address_str}: {instruction}")

            # End current data block if it's in progress
            if current_data_block is not None:
                functions.append(current_data_block)
                current_data_block = None

            # End current other block if it's in progress
            if current_other_block is not None:
                functions.append(current_other_block)
                current_other_block = None

        elif byte_type == "Data":
            instruction = f"data: {opcode}"

            # Start accumulating data bytes
            if current_data_block is None:
                current_data_block = {
                    "type": "Data",
                    "instructions": []
                }
            current_data_block["instructions"].append(f"{address_str}: {instruction}")

            # End the current function if it's in progress
            if current_function is not None:
                functions.append(current_function)
                current_function = None

            # End current other block if it's in progress
            if current_other_block is not None:
                functions.append(current_other_block)
                current_other_block = None

        else:  # byte_type == "Other"
            instruction = f"{opcode}"

            # Start accumulating other bytes
            if current_other_block is None:
                current_other_block = {
                    "type": "Other",
                    "instructions": []
                }
            current_other_block["instructions"].append(f"{address_str}: {instruction}")

            # End the current function if it's in progress
            if current_function is not None:
                functions.append(current_function)
                current_function = None

            # End current data block if it's in progress
            if current_data_block is not None:
                functions.append(current_data_block)
                current_data_block = None

    # Append the last function or data block or other block if they exist
    if current_function is not None:
        functions.append(current_function)
    if current_data_block is not None:
        functions.append(current_data_block)
    if current_other_block is not None:
        functions.append(current_other_block)

    return functions



# Save the Output to JSON
def save_to_json(filename, data):
    with open(filename, 'w') as json_file:
        json.dump(data, json_file, indent=2)

### MAIN EXECUTION
if __name__ == "__main__":
    # Path to your ROM file and CSV file
    rom_path = 'rom.sfc'
    csv_path = 'rom_data_banks.csv'
    
    # Read ROM bytes
    with open(rom_path, 'rb') as rom_file:
        rom_bytes = rom_file.read()
    
    # Load the CSV file
    ranges_list = load_csv_ranges(csv_path)
    
    # Parse the ROM bytes
    output = parse_opcodes(rom_bytes, ranges_list)
    
    # Save the output to a JSON file
    save_to_json('classified_output.json', output)