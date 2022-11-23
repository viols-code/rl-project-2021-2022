# Digital Logic Design Project 2022 at Politecnico di Milano
VHDL hardware description to program an FPGA for a convolutional code 1/2  
Grade: 30L/30  
Prof: Gianluca Palermo

Report: [Report](https://github.com/viols-code/rl-project-2021-2022/blob/main/Design%20documentation.pdf)

## Problem
The specification of the "Final Test (Project of Logical Networks)" 2021/2022 asks for implement an HW module (described in VHDL) that interfaces with a memory. The input module receives a continuous sequence of W words, each of 8 bits, and outputs a continuous sequence of Z words, each of 8 bits. Each of the input words is serialized; in this way a continuous flow U is generated by 1 bit. Convolutional code is applied to this stream ½ (each bit is encoded with 2 bits).  
For a detailed description of the problem: [Specifica](https://github.com/viols-code/rl-project-2021-2022/blob/main/PFRL_Specifica.pdf)

"Convolutional codes are used extensively to achieve reliable data transfer in numerous applications, such as digital video, radio, mobile communications and satellite communications." - Wikipedia
