Function NSCOE(obs As Range, pred As Range, Optional check As Boolean = False) As Double
'Nash Sutcliffe coefficient of efficiency
'Programmed according to equation (2) in
'Legates, D.R. and G.J. McCabe, 1999.  Evaluating the use of "goodness of fit" measures
'in hydrologic and hydroclimatic model variation.  Water Resources Research 35:233-241.
'Implemented by Jim Almendinger, Nov 2009

Dim i As Integer
Dim obsAvg As Double
Dim obsMinusPredSq As Double, obsMinusAvgSq As Double


'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check
'Dim count As Integer
'count = UBound(obsV) - LBound(obsV)
'Use Excel function to get average of observed values
obsAvg = WorksheetFunction.Average(obsV)
'MsgBox "Average obs = " & obsAvg

'Cycle through all values, calculating running sums of squares
obsMinusPredSq = 0
obsMinusAvgSq = 0
For i = 0 To count - 1
    'MsgBox obsV(i)
    'obsMinusPredSq = obsMinusPredSq + (obs(i) - pred(i)) ^ 2
    'obsMinusAvgSq = obsMinusAvgSq + (obs(i) - obsAvg) ^ 2
    obsMinusPredSq = obsMinusPredSq + (obsV(i) - predV(i)) ^ 2
    obsMinusAvgSq = obsMinusAvgSq + (obsV(i) - obsAvg) ^ 2
Next i

'Calculate NSCOE
NSCOE = 1 - (obsMinusPredSq / obsMinusAvgSq)
'MsgBox NSCOE

End Function

Function PERR(obs As Range, pred As Range, Optional check As Boolean = False) As Double
'Percent error
'Implemented by Jim Almendinger, May 2013

Dim i As Integer
Dim obsAvg As Double
Dim obsMinusPredSq As Double, obsMinusAvgSq As Double


'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check

'Use Excel function to get average of both observed & predicted values
obsAvg = WorksheetFunction.Average(obsV)
predAvg = WorksheetFunction.Average(predV)
'MsgBox "Average obs = " & obsAvg
'MsgBox "Average pred = " & predAvg

'Calculate PERR
PERR = 100 * (predAvg - obsAvg) / obsAvg
'MsgBox PERR

End Function

Function PBIAS(obs As Range, pred As Range, Optional check As Boolean = False) As Double
'PBIAS, or percent model bias.
'  Identically the same as -PERR (percent model error),
'  which I calculate much more simply than here,
'  without the need to iterate through a loop
'PBIAS seems like a rather unfortunate measure.
'PERR makes more sense to me and is simpler to calculate.
'Implemented by Jim Almendinger, May 2013

Dim i As Integer
Dim obsSum As Double
Dim obsMinusPredSum As Double


'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check
'count = UBound(obsV) - LBound(obsV)
'Use Excel function to get sum of observed values
obsSum = WorksheetFunction.Sum(obsV)
'MsgBox "Average obs = " & obsAvg

'Cycle through all values, calculating running sums of squares
obsMinusPredSum = 0
'obsMinusAvgSq = 0
For i = 0 To count - 1
    obsMinusPredSum = obsMinusPredSum + (obsV(i) - predV(i))
Next i

'Calculate PBIAS
PBIAS = 100 * obsMinusPredSum / obsSum
'MsgBox PBIAS

End Function
Sub ExtractValue(ByRef obs As Range, ByRef pred As Range, ByRef obsV() As Double, ByRef predV() As Double, ByRef Num As Integer, Optional check As Boolean = False)
    Dim i As Integer, j As Integer
    Num = 0
    If check = True Then
        For i = 1 To obs.count
            If obs(i).Text <> "#N/A" And obs(i).Text <> "#DIV/0!" And obs(i).Text <> "#VALUE!" And obs(i).Text <> "" And pred(i).Text <> "#N/A" And pred(i).Text <> "#DIV/0!" And pred(i).Text <> "#VALUE!" And pred(i).Text <> "" Then
                Num = Num + 1
            End If
        Next i
        ReDim obsV(Num - 1)
        ReDim predV(Num - 1)
        j = 0
        For i = 1 To obs.count
            If obs(i).Text <> "#N/A" And obs(i).Text <> "#DIV/0!" And obs(i).Text <> "#VALUE!" And obs(i).Text <> "" And pred(i).Text <> "#N/A" And pred(i).Text <> "#DIV/0!" And pred(i).Text <> "#VALUE!" And pred(i).Text <> "" Then
                obsV(j) = obs(i)
                predV(j) = pred(i)
                j = j + 1
            End If
        Next i
    End If
    If check = False Then
        Num = obs.count
        ReDim obsV(Num - 1)
        ReDim predV(Num - 1)
        For i = 1 To Num
            obsV(i - 1) = obs(i)
            predV(i - 1) = pred(i)
        Next i
    End If
End Sub
Function RSQUARE(obs As Range, pred As Range, Optional check As Boolean = False) As Double
'Coefficient of determination
'Same as the square of the Pearson correlation coefficient
'  And, the same as the built-in Excel function RSQ()
'Programmed according to equation (1) in
'Legates, D.R. and G.J. McCabe, 1999.  Evaluating the use of "goodness of fit" measures
'in hydrologic and hydroclimatic model variation.  Water Resources Research 35:233-241.
'Implemented by Jim Almendinger, Nov 2009

Dim i As Integer
Dim obsAvg As Double, predAvg As Double
Dim obsMinusAvgSq As Double, predMinusAvgSq As Double, obsPredMinusAvgs As Double


'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check
'count = UBound(obsV) - LBound(obsV)
'Use Excel function to get average of observed values
obsAvg = WorksheetFunction.Average(obsV)
predAvg = WorksheetFunction.Average(predV)
'MsgBox "Average obs = " & obsAvg

'Cycle through all values, calculating running sums of squares
obsMinusAvgSq = 0
predMinusAvgSq = 0
obsPredMinusAvgs = 0

'MsgBox "total number is " & count
For i = 0 To count - 1
    'MsgBox obsV(i) & predV(i)
    'obsMinusAvgSq = obsMinusAvgSq + (obs(i) - obsAvg) ^ 2
    'predMinusAvgSq = predMinusAvgSq + (pred(i) - predAvg) ^ 2
    'obsPredMinusAvgs = obsPredMinusAvgs + (obs(i) - obsAvg) * (pred(i) - predAvg)
    obsMinusAvgSq = obsMinusAvgSq + (obsV(i) - obsAvg) ^ 2
    predMinusAvgSq = predMinusAvgSq + (predV(i) - predAvg) ^ 2
    obsPredMinusAvgs = obsPredMinusAvgs + (obsV(i) - obsAvg) * (predV(i) - predAvg)
Next i

'Calculate RSQUARE
RSQUARE = obsPredMinusAvgs / ((obsMinusAvgSq * predMinusAvgSq) ^ 0.5)
'MsgBox RSQUARE

End Function


Function RSR(obs As Range, pred As Range, Optional check As Boolean = False) As Double
'RMSE-to-SD Ratio
'Programmed according to equation (3) in
'Moriasi et al. 2007.  Model evalutaion guidelines for systematic quantification of accuracy
'  in watershed simulations.  Transactions of the ASABE 50(3): 885-900.

Dim i As Integer
Dim obsAvg As Double
Dim obsMinusAvgSq As Double, obsMinusPredSq As Double


'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check
'count = UBound(obsV) - LBound(obsV)
'Use Excel function to get average of observed values
obsAvg = WorksheetFunction.Average(obsV)
'MsgBox "Average obs = " & obsAvg

'Cycle through all values, calculating running sums of squares
obsMinusPredSq = 0
obsMinusAvgSq = 0
For i = 0 To count - 1
    'obsMinusPredSq = obsMinusPredSq + (obs(i) - pred(i)) ^ 2
    'obsMinusAvgSq = obsMinusAvgSq + (obs(i) - obsAvg) ^ 2
    obsMinusPredSq = obsMinusPredSq + (obsV(i) - predV(i)) ^ 2
    obsMinusAvgSq = obsMinusAvgSq + (obsV(i) - obsAvg) ^ 2
Next i

'Calculate RSR
RSR = obsMinusPredSq ^ 0.5 / obsMinusAvgSq ^ 0.5
'MsgBox RSR

End Function

Function MAE(obs As Range, pred As Range, Optional check As Boolean = False) As Double

Dim i As Integer
Dim obsSum As Double
Dim obsMinusPredSum As Double

'Check to make sure ranges are same length
If obs.count <> pred.count Then
    MsgBox "Number of elements not equal, dumbo.  " & vbCrLf & _
           "Observed = " & obs.count & vbCrLf & _
           "Predicted = " & pred.count
    Exit Function
End If

Dim obsV() As Double
Dim predV() As Double
Dim count As Integer
ExtractValue obs, pred, obsV, predV, count, check
'count = UBound(obsV) - LBound(obsV)
'MsgBox (count)
'Cycle through all values, calculating running sums of squares
obsMinusPredSum = 0
'obsMinusAvgSq = 0
For i = 0 To count - 1
    'MsgBox obsV(i), predV(i)
    obsMinusPredSum = obsMinusPredSum + Abs(obsV(i) - predV(i))
Next i

'Calculate PBIAS
MAE = obsMinusPredSum / count
End Function
