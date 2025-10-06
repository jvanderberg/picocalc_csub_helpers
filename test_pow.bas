' Test program for pow_int CSUB
Option Explicit

CSUB pow_int integer, integer, integer
 00000000
 E9D2B5F0 2E005600 EA55DB25 D01B0306
 3200E9D1 21012400 0701F005 EA45086D
 107675C6 FB01B14F FB03F702 FBA17704
 443C1403 0706EA55 FB03D008 FBA3F702
 EB023203 E7E70247 24002101 1400E9C0
 21002000 2100BDF0 E7F7460C
End CSUB

Dim r%

pow_int r%, 2, 10
Print "2^10 = ", r%

pow_int r%, 5, 5
Print "5^5  = ", r%

pow_int r%, 9, 0
Print "9^0  = ", r%

pow_int r%, -3, 3
Print "(-3)^3 = ", r%

pow_int r%, -3, 4
Print "(-3)^4 = ", r%

End
