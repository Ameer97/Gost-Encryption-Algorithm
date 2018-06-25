Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click


        'Reading Orginal String And General Declaration case ..
        Dim OrginalString As String = TextBox1.Text
        Dim DecimalToBinaryMessage As String = ""
        Dim left As String = ""
        Dim right As String = ""
        Dim Cipher As String = ""
        Dim BinaryKey As String = ""
        Dim AddWithModCase As String = ""
        Dim AfterSBoxCase As String = ""
        Dim shiftingCase As String = ""
        Dim temp As String = ""
        Dim finalResult As String = ""


        'Reading Master Key and Convert to Binary ..
        Dim KeyChunks() As String = KeyFun(TextBox2.Text)


        'Convert Plan Text To Binary ..
        DecimalToBinaryMessage = CharectersToBinary(OrginalString)


        'Inner Structure Algorithm ..
        For i = 1 To 32
            left = Mid(DecimalToBinaryMessage, 1, 32)
            right = Mid(DecimalToBinaryMessage, 33, 32)

            'First Stage: Add with Mod ..
            AddWithModCase = AddWithMod(KeyChunks(ChooseSubKey(i)), right)

            'Second Stage: SBox ..
            AfterSBoxCase = sBox(AddWithModCase)

            'Third Stage: Shifting ..
            shiftingCase = shifting(AfterSBoxCase, 11)

            temp = right
            right = XorFun(shiftingCase, left, 32)
            left = temp
        Next


        'Concat Both ..
        finalResult = left + right
        TextBox3.Text = finalResult

        'Covert Final Cipher To String ..
        TextBox4.Text = AutoConvertBinaryToString(finalResult)

    End Sub

End Class
