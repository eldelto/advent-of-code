package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {

	// Example
	exampleOps := parseInput(exampleInput)
	exampleMachine := NewBitmaskMachine()
	runProgram(exampleMachine, exampleOps)
	fmt.Printf("Example result: %d\n", exampleMachine.GetMemorySum())

	// Scenario 1
	ops := parseInput(input)
	machine := NewBitmaskMachine()
	runProgram(machine, ops)
	fmt.Printf("Scenario 1 result: %d\n", machine.GetMemorySum())

	// Example 2
	exampleOps2 := parseInput(exampleInput2)
	exampleMachine2 := NewBitmaskMachineV2()
	runProgram(exampleMachine2, exampleOps2)
	fmt.Printf("Example 2 result: %d\n", exampleMachine2.GetMemorySum())

	// Scenario 2
	machineV2 := NewBitmaskMachineV2()
	runProgram(machineV2, ops)
	fmt.Printf("Scenario 2 result: %d\n", machineV2.GetMemorySum())
}

func parseInput(input string) []Operation {
	ops := []Operation{}

	rawOps := strings.Split(input, "\n")
	for _, v := range rawOps {
		if strings.Contains(v, "mem[") {
			parts := strings.Split(strings.Split(v, "mem[")[1], "] = ")
			address, err := strconv.ParseUint(parts[0], 10, 64)
			if err != nil {
				panic(fmt.Errorf("not a valid address: %s", parts[0]))
			}
			value, err := strconv.ParseUint(parts[1], 10, 64)
			if err != nil {
				panic(fmt.Errorf("not a valid value: %s", parts[1]))
			}

			op := Operation{
				Type:    WriteToMemory,
				Address: uint64(address),
				Value:   uint64(value),
			}
			ops = append(ops, op)
		} else if strings.Contains(v, "mask") {
			parts := strings.Split(v, "mask = ")

			op := Operation{
				Type:  SetBitmask,
				Value: parts[1],
			}
			ops = append(ops, op)
		} else {
			panic(fmt.Errorf("not a valid operation: %s", v))
		}
	}

	return ops
}

func runProgram(machine Bitmasker, program []Operation) {
	for _, op := range program {
		switch op.Type {
		case WriteToMemory:
			machine.WriteToMemory(op.Address, op.Value.(uint64))
		case SetBitmask:
			machine.SetBitmask(op.Value.(string))
		default:
			panic(fmt.Errorf("unsupported operation: %v", op))
		}
	}
}

type OperationType int

const (
	SetBitmask = OperationType(iota)
	WriteToMemory
)

type Operation struct {
	Type    OperationType
	Address uint64
	Value   interface{}
}

type Memory map[uint64]uint64

type Bitmasker interface {
	SetBitmask(string) error
	WriteToMemory(address, value uint64)
}
type BitmaskMachine struct {
	memory   Memory
	lowMask  uint64
	highMask uint64
}

func NewBitmaskMachine() *BitmaskMachine {
	return &BitmaskMachine{
		memory:   map[uint64]uint64{},
		lowMask:  ^uint64(0),
		highMask: 0,
	}
}

func (bm *BitmaskMachine) SetBitmask(rawBitmask string) error {
	if len(rawBitmask) != 36 {
		return fmt.Errorf("bitmask must be 36 chars long but was: %d", len(rawBitmask))
	}

	newLowMask := uint64(0)
	newHighMask := uint64(0)
	for i, r := range rawBitmask {
		shift := 35 - i
		switch r {
		case '0':
			newLowMask |= (1 << shift)
		case '1':
			newHighMask |= (1 << shift)
		}
	}

	bm.lowMask = ^newLowMask
	bm.highMask = newHighMask

	return nil
}

func (bm *BitmaskMachine) WriteToMemory(address, value uint64) {
	bm.memory[address] = bm.applyBitmask(value)
}

func (bm *BitmaskMachine) GetMemorySum() uint64 {
	sum := uint64(0)
	for _, v := range bm.memory {
		sum += v
	}

	return sum
}

func (bm *BitmaskMachine) applyBitmask(value uint64) uint64 {
	return (value & bm.lowMask) | bm.highMask
}

type BitmaskMachineV2 struct {
	memory   Memory
	highMask uint64
	rawMask  string
}

func NewBitmaskMachineV2() *BitmaskMachineV2 {
	return &BitmaskMachineV2{
		memory:   map[uint64]uint64{},
		highMask: 0,
		rawMask:  "",
	}
}

func (bm *BitmaskMachineV2) SetBitmask(rawBitmask string) error {
	if len(rawBitmask) != 36 {
		return fmt.Errorf("bitmask must be 36 chars long but was: %d", len(rawBitmask))
	}

	newHighMask := uint64(0)
	for i, r := range rawBitmask {
		shift := 35 - i
		if r == '1' {
			newHighMask |= (1 << shift)
		}
	}

	bm.highMask = newHighMask
	bm.rawMask = rawBitmask

	return nil
}

func (bm *BitmaskMachineV2) WriteToMemory(address, value uint64) {
	addresses := bm.applyBitmask(address)
	for _, a := range addresses {
		bm.memory[a] = value
	}
}

func (bm *BitmaskMachineV2) GetMemorySum() uint64 {
	sum := uint64(0)
	for _, v := range bm.memory {
		sum += v
	}

	return sum
}

func (bm *BitmaskMachineV2) applyBitmask(address uint64) []uint64 {
	newAddress := address | bm.highMask

	return applyMemoryDecoder(newAddress, bm.rawMask, 0)
}

func applyMemoryDecoder(address uint64, rawBitmask string, cursor int) []uint64 {
	if cursor >= len(rawBitmask) {
		return []uint64{address}
	}

	if rawBitmask[cursor] != 'X' {
		return applyMemoryDecoder(address, rawBitmask, cursor+1)
	}

	shift := 35 - cursor
	mask := uint64(1) << shift

	otherAddress := address ^ mask

	addresses0 := applyMemoryDecoder(address, rawBitmask, cursor+1)
	addresses1 := applyMemoryDecoder(otherAddress, rawBitmask, cursor+1)

	return append(addresses0, addresses1...)
}

const exampleInput = `mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0`

const exampleInput2 = `mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1`

const input = `mask = 00X10101X110010011XX0X011X100000X010
mem[13197] = 47579321
mem[37394] = 374943
mem[48509] = 418284
mem[29793] = 191792725
mask = 00100X001010010111XX010X10X01100001X
mem[378] = 9637
mem[41083] = 1858241
mem[36394] = 762
mem[40101] = 15295
mem[50670] = 9179863
mem[30384] = 164245
mem[11676] = 393669095
mask = 0XX000X1111001010X10XX1101XX00X00100
mem[17956] = 854
mem[57595] = 179871
mem[50596] = 207644327
mask = X100XX0111X0X1X01101X0X0110010101011
mem[26867] = 1012
mem[3061] = 514891024
mem[35973] = 512
mask = 01X000111110010X1110X00110000X00000X
mem[3707] = 123619
mem[37107] = 7325808
mem[31906] = 102168
mask = XX00XX0101100X011100000XX00100000X00
mem[38336] = 2917310
mem[7166] = 313228
mem[41771] = 2741721
mem[52154] = 89454
mask = 11001111X110X0XX10XX11101111000X0111
mem[35611] = 58725
mem[40227] = 8442431
mem[10029] = 7334
mem[13345] = 87327004
mask = 0XX0X0001010011011010XX0X110XX01X001
mem[4868] = 493430
mem[40452] = 18191
mem[4567] = 330532
mem[13580] = 1662243
mask = 001X00011000X1X0X1X0110X110111001011
mem[2096] = 25174
mem[15283] = 994663544
mem[16002] = 1359
mem[4240] = 704816
mask = 01000011101001001010110X10011X11XX01
mem[16210] = 279177
mem[8996] = 6470256
mem[31049] = 1371071
mem[20425] = 9683526
mem[24986] = 142094045
mask = 0X001X01X1X0110X10XX0X01X00101111010
mem[61471] = 23661
mem[57520] = 246980
mem[57446] = 4169790
mask = 001000X11110010111X000X00X0011X01001
mem[25460] = 1461
mem[528] = 48572826
mem[61442] = 1487
mem[60708] = 1804
mem[51588] = 36111
mask = 0X000001111001010X1011100100001X0X0X
mem[45713] = 3807909
mem[26867] = 6236
mem[39186] = 3023142
mem[46212] = 5671589
mem[28686] = 10375260
mask = 00X0000XXX10X1011100X100001X0X100010
mem[7272] = 1073
mem[35482] = 1877478
mask = 01X00XX11110X11X110X1000010XXX101110
mem[28892] = 2154970
mem[33403] = 34104
mem[56124] = 11230
mem[11265] = 484054168
mem[36422] = 1080979
mask = 00000001X11001X1110001X01000101XXXX0
mem[8445] = 2387
mem[55775] = 8215
mask = 110001101110110X10010X00XX1X01010100
mem[49202] = 4152007
mem[49938] = 4676666
mem[58184] = 43886544
mem[35840] = 8484248
mem[25642] = 1980350
mem[37918] = 1692542
mem[44918] = 43860354
mask = 0X000X011110010X1100X10X100X1X111100
mem[24669] = 15051033
mem[43432] = 1054
mem[64706] = 21834
mem[27336] = 773822
mem[24406] = 420
mask = 00XX01111X1001001X0001101001111101XX
mem[48221] = 256859
mem[1361] = 145480785
mem[8221] = 32962081
mem[11649] = 327597066
mem[6355] = 3866
mem[33696] = 951794
mask = X10XX0010110011010111100X010X000000X
mem[51014] = 577
mem[38304] = 7416
mem[34256] = 3748
mem[12926] = 306
mem[39297] = 1823498
mask = 0000X00XXX10011XX000000X10X010100001
mem[18788] = 7298
mem[39644] = 7675
mem[33093] = 416832
mem[5195] = 6722351
mem[28000] = 53707945
mem[5336] = 77314873
mem[992] = 134079888
mask = 0000X101110X010011X0000000X011X1X101
mem[8536] = 401336
mem[1632] = 1267
mem[6456] = 581
mem[31682] = 20542
mem[27729] = 726
mem[49447] = 37811
mask = 0X0X00111XX001XX111X1011010X100X0111
mem[38677] = 9808
mem[54045] = 28545
mem[30384] = 49811
mem[62515] = 19422
mem[14508] = 49363
mask = X100X1X1011X1111011X01101000X11XX101
mem[59468] = 387
mem[37138] = 121701873
mem[18728] = 2624430
mem[28293] = 5039090
mem[32321] = 839768
mem[28535] = 647
mask = XX100X0010100XX1110X11000110X100X00X
mem[2147] = 1562
mem[25832] = 275992750
mask = 110XX0X10110X11X10X1110X10100X000000
mem[22152] = 470276298
mem[60732] = 716
mem[52727] = 1427169
mem[16196] = 9596060
mem[3910] = 28432777
mask = 01X0001111100X101110110X011XXX01X011
mem[8221] = 60956
mem[1223] = 293
mem[24130] = 777493
mem[62002] = 3900
mem[20316] = 26105612
mem[58295] = 528344031
mem[12465] = 1159
mask = 11000X011110010010011100100XX111100X
mem[59580] = 3601634
mem[31173] = 325319
mem[28284] = 2285676
mem[57595] = 8662764
mem[32672] = 4111107
mask = 01000001X1100101XX0X01101X1X00011X0X
mem[33093] = 4612
mem[61749] = 5692
mem[28188] = 26420039
mem[59026] = 1325
mem[37032] = 149
mask = 01000X11111001X11110011X111X01100X01
mem[23793] = 131971
mem[10420] = 848652
mem[10469] = 33394600
mem[23001] = 1875
mask = 01001111011X1X1XXX1X001XX11100011X10
mem[38294] = 371641
mem[17261] = 72227824
mem[41143] = 213405737
mem[54246] = 1445548
mem[41901] = 1849824
mask = 0X00110X01101100XX101011001110XX1101
mem[42925] = 176752065
mem[17255] = 12010392
mem[37437] = 64942
mem[57253] = 827582
mask = 11X10X101111XX0010X10101100100010110
mem[14344] = 6638562
mem[40266] = 89477554
mem[9511] = 1133
mem[53072] = 748643130
mem[4567] = 372
mem[26565] = 4739
mask = 00010XX1X11X010X11X000100111001X1110
mem[4247] = 17837
mem[18674] = 2031700
mem[47487] = 89684677
mask = 110X1X11011X101110111101XX01001100X0
mem[22046] = 3445
mem[55657] = 29187
mem[38510] = 159862
mask = 0100000X11X00100110X01101X0011001100
mem[12772] = 3394234
mem[30255] = 47301946
mem[55181] = 11825301
mem[2096] = 239
mem[6355] = 66264
mem[62103] = 62751352
mask = X100010XXX10X1001110001001X10X100X0X
mem[18880] = 12756
mem[52727] = 58449492
mem[52620] = 106260805
mask = 01000X011110010XX110X11001100X000XXX
mem[5734] = 3770
mem[46419] = 12806425
mem[26866] = 6566928
mask = X1001XX101101X1X10111X0X1111001101X0
mem[54847] = 8045
mem[61036] = 1672
mem[32929] = 87423486
mem[48539] = 13896
mem[14353] = 7009065
mem[16446] = 32987614
mask = 00100000X111110X1110X101100000100X00
mem[23863] = 4070
mem[52234] = 498973
mem[20235] = 1706949
mask = 00X000011110X100110X11100X0X10X01010
mem[44286] = 26378
mem[30741] = 14368000
mem[7396] = 88194752
mask = 0X001X01X11011X0101X1010XX01X01000X0
mem[6834] = 2670580
mem[15903] = 6945101
mem[10104] = 2517
mem[65020] = 61602027
mem[55142] = 1143278
mem[38596] = 1333
mem[17509] = 153504934
mask = 00XX0X01111XX10111X00X10X0111011110X
mem[568] = 1625
mem[30255] = 248279
mem[11912] = 538
mem[12028] = 520033
mask = 0010XX011X00X10011X001XX1100X1000001
mem[15654] = 120
mem[4292] = 57554
mem[2096] = 346664
mem[1248] = 10995
mem[48539] = 656539
mem[19181] = 147211
mask = 1010010X110001001100X1X00X0011010X01
mem[11344] = 4845730
mem[60639] = 25030928
mem[1005] = 12960528
mem[47688] = 107547855
mem[17509] = 728745
mask = X00000011X10X10X11000101101000000100
mem[46621] = 1608
mem[5562] = 15064109
mem[28385] = 1651
mem[5734] = 288099689
mem[49319] = 97558812
mem[37739] = 1161
mem[52727] = 1992490
mask = 0001010XX110010X110000X01X10X011XX1X
mem[37366] = 735085354
mem[1739] = 409
mem[12604] = 754989
mask = 00000X011110010011000X001X0100XX0000
mem[29851] = 8508
mem[44133] = 1998890
mem[24804] = 6068
mem[59260] = 1198469
mem[59580] = 10413954
mask = 00110111X01X010010X00000X10X00110110
mem[55579] = 418284890
mem[7573] = 1800
mem[26096] = 1469134
mem[44133] = 25531796
mem[39628] = 7076590
mask = 0000000111X0010111X001011XX0000X1X00
mem[24778] = 24875864
mem[10420] = 3072761
mem[2096] = 2762
mem[30148] = 6696
mem[17770] = 13204967
mask = 0011010101X00X0X1100X000110011100101
mem[57742] = 21044093
mem[33266] = 56667
mem[61442] = 1636
mem[51870] = 1018197644
mem[40469] = 77760
mask = X0X001XX11000X0011X00100010011X00X01
mem[11504] = 438885
mem[53777] = 6130
mem[17770] = 808014
mask = 000X001111100101XX100010X010X1100100
mem[35611] = 1215
mem[10397] = 301
mem[10866] = 8375216
mem[2499] = 10472
mem[63504] = 270966
mem[7719] = 2064
mem[41088] = 343764913
mask = 010X1X00X1X0X1101X0X00111101001X0101
mem[20597] = 773
mem[14657] = 294987038
mask = 00XX0001X110010111100000X01010100111
mem[43170] = 365
mem[55657] = 4882105
mem[44170] = 628956
mem[10429] = 947957
mem[272] = 36059
mask = 0X000011X0X0010X1X1011110100101XXXX1
mem[3910] = 2312
mem[47735] = 89421
mem[2197] = 32972549
mem[49938] = 7005
mask = 00000100X100X100111X0101011X11010101
mem[54400] = 830
mem[43977] = 2412350
mem[18229] = 19157634
mem[19181] = 879884567
mask = 000101X11X100100X100011X000100110001
mem[37237] = 104037633
mem[29167] = 1094
mem[6308] = 6007
mem[21610] = 80462437
mem[2261] = 345450311
mask = 0X0000X1111001011110X01X1XX01010X0XX
mem[10892] = 85035
mem[38510] = 42597708
mem[30752] = 2960
mem[30416] = 123661909
mem[49045] = 913
mem[4292] = 75
mem[17509] = 1382
mask = 1X00X00111X001X0100101001X110X0001X0
mem[56241] = 8276
mem[40782] = 243373
mem[42397] = 43237586
mem[13015] = 399
mem[23129] = 735838
mem[1385] = 4448
mask = 0000X011000XX100X110111101X110X010X1
mem[13015] = 247
mem[42378] = 546670
mem[8214] = 63809
mem[22410] = 155149
mem[55264] = 454543598
mem[28778] = 69088880
mem[48061] = 12489909
mask = 0XX0X000XX1001X01101001000000100X1X1
mem[7551] = 11717790
mem[6107] = 50467
mem[30752] = 4249755
mem[17228] = 33884
mem[59957] = 32163540
mask = 1X0X0X10111X01001X0101011X0X0X0101X0
mem[45349] = 1879078
mem[31733] = 88189
mem[12577] = 546676501
mem[53419] = 16199
mem[59921] = 2546
mask = 0010001X110001X1X110100101X111000000
mem[19998] = 179736
mem[22061] = 30001652
mem[41371] = 16212851
mem[64355] = 622251640
mem[23129] = 98852
mem[56429] = 266
mask = 00000X0X11X00XX1100001X0100010X0X100
mem[37241] = 450
mem[64833] = 219911344
mem[16713] = 626602448
mask = X1X01011X11010111011XX000001X01XX100
mem[47336] = 15388
mem[56124] = 582872
mem[28686] = 27406190
mem[34623] = 13146
mem[30384] = 229541309
mask = X00X011X11100100110X00110001XXX00000
mem[18660] = 3353
mem[30741] = 10036798
mem[47487] = 4090
mask = 00X0000X11100X011X0001101001X10X10X0
mem[52567] = 730515
mem[16156] = 10615261
mem[29580] = 957414770
mem[26765] = 19
mem[24655] = 510031974
mem[24669] = 30
mem[46781] = 5769
mask = 0X0010011110111010X000X0X11X101001X0
mem[63285] = 8050
mem[46893] = 981797
mem[46022] = 984778
mem[31931] = 67780
mem[23543] = 3153
mask = 1100X001X11001X01001X100111X01X0X100
mem[53172] = 13616323
mem[31500] = 117639
mem[59037] = 106552607
mem[54562] = 3408946
mem[57159] = 411910
mem[2824] = 61677145
mem[47313] = 520051559
mask = X100010111100100XX0XX110100001010101
mem[37739] = 3912032
mem[12772] = 884026
mem[2217] = 4238
mem[57159] = 28391
mem[17146] = 3998
mask = 00000001111001X11X000110101X1X11X100
mem[11649] = 516652
mem[21190] = 526
mem[6364] = 274816
mem[25567] = 190071
mask = XXX00000X111010111001001111000X00000
mem[25786] = 934
mem[22026] = 215203
mask = 0010000110X0110X11X001001101110X0011
mem[59392] = 400
mem[59580] = 927
mem[15208] = 18515970
mem[59444] = 245687
mem[36248] = 893
mem[59134] = 20240
mask = 0X0XX1X11110X1011010000110000010000X
mem[11344] = 223586488
mem[41434] = 519191086
mem[15028] = 1492592
mem[25631] = 247281
mask = 11X0XX1011101000X0011X00X101X1111000
mem[21825] = 1675
mem[2371] = 663473
mem[37724] = 4339
mem[54246] = 855
mem[9392] = 56579
mem[35552] = 14454
mask = 0100010111100100111X110X00X01000X1X0
mem[44516] = 482691
mem[26528] = 1559
mem[53886] = 89643332
mem[3478] = 12699372
mask = 0011010111X00100XXX0XX1X10XX10100001
mem[27239] = 413074512
mem[15589] = 3001
mem[10011] = 1136
mem[59026] = 852921
mem[5283] = 213015528
mask = 00X0000XX11XX10111X0000001X000101000
mem[17298] = 4088539
mem[54369] = 505389469
mem[8221] = 132829
mem[55115] = 5055
mem[56330] = 1932140
mask = 01X00001111001010110111001X00010X0XX
mem[37467] = 127273625
mem[38432] = 3166735
mem[52727] = 998
mem[42925] = 1733202
mask = 11010011011011X01X01010XX10110110000
mem[26085] = 233531
mem[57079] = 1526
mem[53172] = 385838419
mem[29614] = 254712697
mem[25466] = 735
mem[27081] = 7719
mask = 01000000111X0XXX1101X11X001101X01100
mem[55941] = 72432190
mem[16199] = 10097889
mem[45349] = 10650
mem[62515] = 8214
mem[59468] = 965897
mem[16653] = 386447843
mem[49447] = 413274
mask = 01000X011110X1XX1XX10X001111000X1000
mem[59209] = 14316
mem[50790] = 705222705
mem[45621] = 232211796
mask = 010X111101111XX1101000X1011000111011
mem[48891] = 3231894
mem[37241] = 49993
mem[47016] = 3806
mem[10469] = 12758
mem[43871] = 17980
mask = 0XX00XX11110010011X0010X1X011100X000
mem[28733] = 801341049
mem[12772] = 20002640
mem[30741] = 2248
mask = 0101X01110100X0111XX11110011000XXX01
mem[17803] = 65414
mem[45439] = 13886371
mem[16184] = 130293
mem[29313] = 1782
mask = 01000111111001101X0110000001XX1X1111
mem[34404] = 202755
mem[43413] = 1045
mem[48841] = 381434549
mem[37238] = 230052
mem[10175] = 421554
mem[11939] = 15850
mask = X0XX0011X110010X011000010111110X1XX0
mem[54847] = 160031
mem[53915] = 5111
mem[6036] = 17132
mem[36183] = 1360055
mem[47658] = 127563
mask = 00100X1111X0010101101X11X11111001X10
mem[25937] = 8673
mem[12926] = 834
mem[43356] = 732346
mem[5734] = 393
mem[3822] = 30383
mask = 110001101110XX00100100XX0X11X1XX010X
mem[821] = 185158312
mem[64509] = 264189683
mem[15199] = 1037709
mem[635] = 437556
mem[31141] = 14726605
mem[40468] = 47196
mask = 00000001XX1001001X0000X01001X0001100
mem[46740] = 8071
mem[43144] = 14187193
mem[51555] = 382
mem[30741] = 4087
mem[45411] = 1430
mem[14377] = 7364
mask = 11X00110111001001001100X10X1XX00X100
mem[62803] = 979004797
mem[4838] = 343
mem[23241] = 4426
mask = 001X0101X1100100X1X0X110110XX01001X0
mem[10799] = 5745
mem[14789] = 34632
mem[27068] = 8863
mask = 001001011XX0010011000111010X10XX0100
mem[31173] = 109895
mem[25936] = 4493
mask = 00000011111X0101X1X01100X00000100000
mem[32612] = 14472
mem[22702] = 713
mem[22779] = 126887839
mem[43168] = 251908
mem[11504] = 47004
mem[7399] = 2215129
mem[63129] = 4367
mask = 010001010X10010X111XX1X0111100101X0X
mem[3663] = 187207677
mem[52928] = 200
mem[38852] = 8397
mem[31461] = 223838656
mem[64706] = 4205924
mem[26237] = 218385741
mem[57402] = 258615472
mask = 11X0X0X1011001XX1001111X011100XX0000
mem[55334] = 323423
mem[5833] = 33097
mem[26924] = 272
mem[42957] = 47976862
mem[18798] = 61419
mem[19947] = 1287
mask = 1100X101011X10111X11X000111100111XX0
mem[64472] = 224734
mem[18079] = 526
mem[60870] = 3322170
mem[4013] = 578
mem[2606] = 83684765
mem[17048] = 2503
mask = 0010000XX0000100110000011X0001X0X00X
mem[37789] = 94286
mem[2499] = 13318890
mem[29793] = 78507
mem[45621] = 3626886
mem[34770] = 2407
mem[54400] = 38740
mask = 0010X10111000100110001X0X1000100X101
mem[11265] = 1297549
mem[16410] = 948310
mem[33093] = 893977
mem[17298] = 53047
mem[45439] = 14869
mem[21212] = 870451788`
