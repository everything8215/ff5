package main

import (
"fmt"
"os"
)

const StartPos = 0x20000
const BytesToCheck = 0x9FFF

func main() {
	genFile, _ := os.ReadFile("../rom/ff5-en.sfc")
	origFile, _ := os.ReadFile("../rom/Final Fantasy 5.sfc")

	// How many addresses you want to see at a time
	top := 5
	// In case you want to skip a few mistakes
	skipFirst := 0
	fmt.Printf("Skipped first %d mistakes\n", skipFirst)

	hasErrors := false

	for i := StartPos; i < StartPos + BytesToCheck; i++ {
		if genFile[i] != origFile[i] {
			hasErrors = true
			if skipFirst > 0 {
				skipFirst--
				continue
			}
			fmt.Printf("Files differ at %04X\n", i - StartPos)
			fmt.Printf("Gen: %02X, Orig: %02X\n", genFile[i], origFile[i])
			top--
			if top == 0 {
				break
			}
		}
	}
	fmt.Printf("ROM Matches from 0x%06X-0x%06X: %t\n", StartPos, StartPos+BytesToCheck, !hasErrors)
}