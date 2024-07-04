package main

import (
"fmt"
"os"
)

func main() {
	genFile, _ := os.ReadFile("../rom/ff5-en.sfc")
	origFile, _ := os.ReadFile("../rom/Final Fantasy 5.sfc")
	startPos := 0x020000
	//fmt.Println(origFile[startPos])
	//fmt.Println(genFile[startPos])

	top := 5
	//top := 0
	skipFirst := 0
	fmt.Printf("Skipped first %d mistakes\n", skipFirst)

	for i := startPos; i < startPos + 0x6850; i++ {
		if genFile[i] != origFile[i] {
			if skipFirst > 0 {
				skipFirst--
				continue
			}
			//fmt.Println("Sheesh")
			fmt.Printf("Files differ at %4X\n", i - startPos)
			fmt.Printf("Gen: %2X, Orig: %2X\n", genFile[i], origFile[i])
			top--
			if top == 0 {
				break
			}
		}
	}
	fmt.Println(top)
}