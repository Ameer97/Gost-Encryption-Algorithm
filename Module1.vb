Module Module1


    Function sBox(Mystring As String)

        Dim s1 As String = "4 10 9 2 13 8 0 14 6 11 1 12 7 15 5 3"
        Dim s2 As String = "14 11 4 12 6 13 15 10 2 3 8 1 0 7 5 9"
        Dim s3 As String = "5 8 1 13 10 3 4 2 14 15 12 7 6 0 9 11"
        Dim s4 As String = "7 13 10 1 0 8 9 15 14 4 6 12 11 2 5 3"
        Dim s5 As String = "6 12 7 1 5 15 13 8 4 10 9 14 0 3 11 2"
        Dim s6 As String = "4 11 10 0 7 2 1 13 3 6 8 5 9 12 15 14"
        Dim s7 As String = "13 11 4 1 3 15 5 9 0 10 14 7 6 8 2 12"
        Dim s8 As String = "1 15 13 0 5 7 10 4 9 2 3 14 6 11 8 12"


        Dim a1() = s1.Split(" ")
        Dim a2() = s2.Split(" ")
        Dim a3() = s3.Split(" ")
        Dim a4() = s4.Split(" ")
        Dim a5() = s5.Split(" ")
        Dim a6() = s6.Split(" ")
        Dim a7() = s7.Split(" ")
        Dim a8() = s8.Split(" ")




        Dim chunks(7) As String
        Dim chunksBinary(7) As String
        Dim counter As Integer = 0
        Dim ArrayInt(7) As Integer
        Dim ArrayString(7) As Integer
        For i = 1 To 32 Step 8
            chunksBinary(counter) = Mid(Mystring, i, 4)
            chunks(counter) = BtoD(chunksBinary(counter), 4)
            counter += 1
        Next





        Dim c(7) As Integer
        c(0) = a1(chunks(0))
        c(1) = a2(chunks(1))
        c(2) = a3(chunks(2))
        c(3) = a4(chunks(3))
        c(4) = a5(chunks(4))
        c(5) = a6(chunks(5))
        c(6) = a7(chunks(6))
        c(7) = a8(chunks(7))


        Dim cBinary(7) As String
        For i = 0 To 7
            cBinary(i) = DtoB(c(i), 4)
        Next

        Return Join(cBinary, "")
    End Function

    Function CharectersToBinary(MyString As String)
        Dim midC As Char
        Dim ascycode As Integer
        Dim decimaelToBinary As String = ""

        For i = 1 To 8
            midC = Mid(MyString, i, 1)
            ascycode = Asc(midC)
            decimaelToBinary += DtoB(ascycode, 8)
        Next
        Return decimaelToBinary
    End Function

    Function DtoB(val As Integer, TheLength As Integer)
        Dim stack As String = ""

        While val <> 0
            If val Mod 2 = 0 Then
                stack = "0" + stack
            Else
                stack = "1" + stack
            End If
            val = val \ 2
        End While
        While stack.Length < TheLength
            stack = "0" + stack
        End While

        Return stack
    End Function


    Function BtoD(MyString As String, TheLength As Integer)

        Dim ArrString(MyString.Length) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(TheLength) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To MyString.Length Step TheLength
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(MyString, i, TheLength)
            For j = 1 To TheLength
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To TheLength
                StackInteger += Math.Pow(2, TheLength - j) * splitbit(j)
            Next
        Next


        Return StackInteger
    End Function

    Function AddWithMod(firstValue As String, SecondValue As String)
        Dim chunks(3) As Integer
        Dim chunksBinary(3) As String
        Dim FirstInt As Integer
        Dim SecondInt As Integer
        Dim counter As Integer = 0

        For i = 1 To 32 Step 8
            FirstInt = BtoD(Mid(firstValue, i, 8), 8)
            SecondInt = BtoD(Mid(SecondValue, i, 8), 8)
            chunks(counter) = (FirstInt + SecondInt) Mod 256
            counter += 1
        Next

        For i = 0 To 3
            chunksBinary(i) = DtoB(chunks(i), 8)
        Next


        Return Join(chunksBinary, "")
    End Function

    Function shifting(MyString As String, NumberOfShinftingLoop As Integer)
        Dim EditOnMyString(MyString.Length) As String

        For i = 0 To MyString.Length - 1
            EditOnMyString(i) = MyString(i)
        Next


        Dim TheFirstBit As String = ""

        For i = 0 To NumberOfShinftingLoop
            TheFirstBit = EditOnMyString(0)

            For j = 1 To EditOnMyString.Length - 1
                EditOnMyString(j - 1) = EditOnMyString(j)
            Next
            EditOnMyString(EditOnMyString.Length - 1) = TheFirstBit
        Next
        Return Join(EditOnMyString, "")
    End Function


    Function XorFun(ByVal first As String, ByVal second As String, ByVal index As Integer)
        Dim ConvertToIntFirst As Integer = 0
        Dim ConvertToIntSecond As Integer = 0
        Dim FinalReturn(index) As String
        Dim spliterFirst As String = 0
        Dim spliterSecond As String = 0

        For i = 1 To index

            spliterFirst = Mid(first, i, 1)
            ConvertToIntFirst = Convert.ToInt32(spliterFirst)

            spliterSecond = Mid(second, i, 1)
            ConvertToIntSecond = Convert.ToInt32(spliterSecond)

            FinalReturn(i) = (ConvertToIntFirst Xor ConvertToIntSecond).ToString()

        Next
        Return Join(FinalReturn, "")


    End Function

    Function AutoConvertBinaryToString(ByVal val As String)

        Dim ArrString(val.Length) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(8) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To val.Length Step 8
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(val, i, 8)
            For j = 1 To 8
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To 8
                StackInteger += Math.Pow(2, 8 - j) * splitbit(j)
            Next
            DtoC += Chr(StackInteger)
        Next


        Return DtoC
    End Function

    Function KeyFun(MyKey As String)
        MyKey = CharectersToBinary(MyKey)
        MyKey = CharectersToBinary(MyKey)
        While MyKey.Length < 256
            MyKey = MyKey + "00110000"
        End While

        Dim KeyChunks(7) As String
        Dim counter As Integer = 0
        For i = 1 To 256 Step 32
            KeyChunks(counter) = Mid(MyKey, i, 32)
            counter += 1
        Next



        Return KeyChunks
    End Function

    Function ChooseSubKey(RoundIndex As Integer)

        If RoundIndex >= 1 AndAlso RoundIndex <= 8 Then
            Return RoundIndex - 1
        End If

        If RoundIndex >= 9 AndAlso RoundIndex <= 16 Then
            Return RoundIndex - 9
        End If

        If RoundIndex >= 17 AndAlso RoundIndex <= 24 Then
            Return RoundIndex - 17
        End If

        If RoundIndex >= 25 AndAlso RoundIndex <= 32 Then
            Return RoundIndex - 25
        End If


    End Function

End Module
