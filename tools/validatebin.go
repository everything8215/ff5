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

	for i := startPos; i < startPos + 0x3970; i++ {
		if genFile[i] != origFile[i] {
			//fmt.Println("Sheesh")
			fmt.Printf("Files differ at %4X\n", i)
			fmt.Printf("Gen: %2X, Orig: %2X\n", genFile[i], origFile[i])
			top--
			if top == 0 {
				break
			}
		}
	}
}