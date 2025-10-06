' Mandelbrot renderer with menu (PicoMite MMBasic) — interruptible render
OPTION EXPLICIT
OPTION CONTINUATION LINES ON

CSUB mandelbrot integer, integer, integer, integer, integer, integer
 00000000
 4FF0E92D E9DDB08D E9D66516 2C014600
 0600F176 682EDB7D 2E01686D 9D039503
 F1759602 DB740500 9B00E9D1 68526815
 92089507 685B681A 930A9209 03C4EB00
 21009306 460C468A 460F460E 468C460A
 468E460B 18DB9101 FC0CFB03 0E0EEB4E
 CC0EFB02 2302FBA3 44639D07 EA420F12
 19521203 EB459D08 9B017C23 EB671B34
 EB140503 EB4B0309 FB030E05 FBA3F40E
 FB026703 EB07F80C FBA20744 31014502
 0548EB05 EA4F9104 F14A7414 EA440100
 91051405 9501172D 1A04E9DD 5802E9DD
 BF0845D0 EA4F428D EA467616 EA4F1607
 D00D7727 46A81935 EB479D01 950B0505
 F8DF4645 45458038 F1759D0B DBB20500
 30089B04 3C08F840 F8409B05 9B093C04
 0309EB13 9B0A4699 0B0BEB43 42839B06
 2000D197 B00D2100 8FF0E8BD 40000001
End CSUB

Const SCALE = 268435456            ' 1 << 28 fixed point
Const SCREEN_W = 320
Const SCREEN_H = 320
Const MENU_H = 16
Const MANDEL_W = SCREEN_W
Const MANDEL_H = SCREEN_H - MENU_H
Const MANDEL_TOP = MENU_H
Const BLOCK_W = 64
Const X_MIN_DEF = -2.0
Const X_MAX_DEF = 1.0
Const Y_MIN_DEF = -1.5
Const Y_MAX_DEF = 1.5
Const MAX_ITER = 64
Const STACK_MAX = 16
Const MENU_BG = RGB(40, 40, 40)
Const MENU_FG = RGB(255, 255, 255)
Const CURSOR_CLR = RGB(255, 255, 0)
Const OVERLAY_MAX = MANDEL_W * 4

Dim colors%(255)
Dim block%(BLOCK_W - 1)
Dim startX%
Dim startY%
Dim delta%
Dim x%
Dim y%
Dim bCount%
Dim i%
Dim iter%
Dim dx As Float
Dim dy As Float
Dim g As Integer
Dim rc%
Dim gc%
Dim bc%
Dim xMin As Float
Dim xMax As Float
Dim yMin As Float
Dim yMax As Float
Dim currDX As Float
Dim currDY As Float
Dim xHist(STACK_MAX - 1) As Float
Dim xHistMax(STACK_MAX - 1) As Float
Dim yHist(STACK_MAX - 1) As Float
Dim yHistMax(STACK_MAX - 1) As Float
Dim stackPtr As Integer
Dim running As Integer
Dim keyCode As Integer
Dim overlayX%(OVERLAY_MAX - 1)
Dim overlayY%(OVERLAY_MAX - 1)
Dim overlayClr%(OVERLAY_MAX - 1)
Dim overlayCnt As Integer
Dim pendingKey%        ' -1 means none pending

pendingKey% = -1

' Non-blocking key read: returns -1 if no key waiting
Function ConsumeKeyIfAny() As Integer
  Local k$
  k$ = INKEY$
  If k$ = "" Then
    ConsumeKeyIfAny = -1
  Else
    ConsumeKeyIfAny = ASC(k$)
  EndIf
End Function

' Blocking key read
Function GetKey() As Integer
  Local k$
  Do
    k$ = INKEY$
  Loop Until k$ <> ""
  GetKey = ASC(k$)
End Function

Function Fix28(v As Float) As Integer
    If v >= 0 Then
        Fix28 = Int(v * SCALE + 0.5)
    Else
        Fix28 = -Int(-v * SCALE + 0.5)
    EndIf
End Function

Function ClampInt(v As Integer, lo As Integer, hi As Integer) As Integer
    If v < lo Then
        ClampInt = lo
    ElseIf v > hi Then
        ClampInt = hi
    Else
        ClampInt = v
    EndIf
End Function

Sub PushViewport()
    If stackPtr < STACK_MAX Then
        xHist(stackPtr) = xMin
        xHistMax(stackPtr) = xMax
        yHist(stackPtr) = yMin
        yHistMax(stackPtr) = yMax
        stackPtr = stackPtr + 1
    EndIf
End Sub

Function PopViewport() As Integer
    If stackPtr > 0 Then
        stackPtr = stackPtr - 1
        xMin = xHist(stackPtr)
        xMax = xHistMax(stackPtr)
        yMin = yHist(stackPtr)
        yMax = yHistMax(stackPtr)
        PopViewport = 1
    Else
        PopViewport = 0
    EndIf
End Function

Sub ResetViewport()
    xMin = X_MIN_DEF
    xMax = X_MAX_DEF
    yMin = Y_MIN_DEF
    yMax = Y_MAX_DEF
    stackPtr = 0
End Sub

Sub DrawMenu()
    ' BOX uses width/height (W,H), not x2/y2
    Box 0, 0, SCREEN_W, MENU_H, 1, MENU_BG, MENU_BG
    ' TEXT needs a string; leave optional args blank, set colours
    Text 6, 2, "Z=Zoom  O=Back  R=Reset  Esc=Quit", , , , MENU_FG, MENU_BG
End Sub

Sub DrawZoomBox(boxX As Integer, boxY As Integer, boxSize As Integer)
    Local idx As Integer
    Local xPix As Integer
    Local yPix As Integer

    idx = 0
    For xPix = boxX To boxX + boxSize - 1
        overlayX%(idx) = xPix
        overlayY%(idx) = boxY
        overlayClr%(idx) = Pixel(xPix, boxY)
        Pixel xPix, boxY, CURSOR_CLR
        idx = idx + 1
    Next xPix
    For xPix = boxX To boxX + boxSize - 1
        overlayX%(idx) = xPix
        overlayY%(idx) = boxY + boxSize - 1
        overlayClr%(idx) = Pixel(xPix, boxY + boxSize - 1)
        Pixel xPix, boxY + boxSize - 1, CURSOR_CLR
        idx = idx + 1
    Next xPix
    For yPix = boxY + 1 To boxY + boxSize - 2
        overlayX%(idx) = boxX
        overlayY%(idx) = yPix
        overlayClr%(idx) = Pixel(boxX, yPix)
        Pixel boxX, yPix, CURSOR_CLR
        idx = idx + 1
        overlayX%(idx) = boxX + boxSize - 1
        overlayY%(idx) = yPix
        overlayClr%(idx) = Pixel(boxX + boxSize - 1, yPix)
        Pixel boxX + boxSize - 1, yPix, CURSOR_CLR
        idx = idx + 1
    Next yPix
    overlayCnt = idx
End Sub

Sub UndrawZoomBox()
    Local idx As Integer
    For idx = 0 To overlayCnt - 1
        Pixel overlayX%(idx), overlayY%(idx), overlayClr%(idx)
    Next idx
    overlayCnt = 0
End Sub

' Interruptible renderer: checks for key every ~10 lines.
Sub RenderViewport()
    Local rX As Integer
    Local rY As Integer
    Local linesSinceCheck As Integer

    If MANDEL_W > 1 Then
        dx = (xMax - xMin) / (MANDEL_W - 1)
    Else
        dx = 0
    EndIf
    If MANDEL_H > 1 Then
        dy = (yMax - yMin) / (MANDEL_H - 1)
    Else
        dy = 0
    EndIf
    currDX = dx
    currDY = dy

    delta% = Fix28(dx)
    If delta% = 0 Then delta% = 1

    linesSinceCheck = 0
    For rY = 0 To MANDEL_H - 1
        startY% = Fix28(yMin + dy * rY)

        ' Every ~10 lines, see if user pressed a key; if so, stash it and exit.
        linesSinceCheck = linesSinceCheck + 1
        If linesSinceCheck >= 10 Then
            pendingKey% = ConsumeKeyIfAny()
            If pendingKey% <> -1 Then Exit Sub
            linesSinceCheck = 0
        EndIf

        For rX = 0 To MANDEL_W - 1 Step BLOCK_W
            bCount% = MANDEL_W - rX
            If bCount% > BLOCK_W Then bCount% = BLOCK_W
            startX% = Fix28(xMin + dx * rX)
            mandelbrot block%(0), startX%, startY%, delta%, bCount%, MAX_ITER
            For i% = 0 To bCount% - 1
                iter% = block%(i%)
                If iter% > MAX_ITER Then iter% = MAX_ITER
                g = Int(255 * iter% / MAX_ITER)
                Pixel rX + i%, rY + MANDEL_TOP, colors%(g)
            Next i%
        Next rX
    Next rY
End Sub

Sub DoZoom()
    Local boxSize As Integer
    Local moveStep As Integer
    Local boxX As Integer
    Local boxY As Integer
    Local done As Integer
    Local key As Integer
    Local pixLeft As Integer
    Local pixTop As Integer
    Local pixRight As Integer
    Local pixBottom As Integer
    Local newXMin As Float
    Local newXMax As Float
    Local newYMin As Float
    Local newYMax As Float

    boxSize = MANDEL_W \ 3
    If boxSize < 12 Then boxSize = 12
    moveStep = ClampInt(boxSize \ 6, 1, boxSize)
    boxX = (MANDEL_W - boxSize) \ 2
    boxY = MANDEL_TOP + (MANDEL_H - boxSize) \ 2

    DrawZoomBox boxX, boxY, boxSize
    done = 0
    Do While done = 0
        key = GetKey()
        Select Case key
            Case 128 ' Up
                UndrawZoomBox
                boxY = ClampInt(boxY - moveStep, MANDEL_TOP, MANDEL_TOP + MANDEL_H - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 129 ' Down
                UndrawZoomBox
                boxY = ClampInt(boxY + moveStep, MANDEL_TOP, MANDEL_TOP + MANDEL_H - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 130 ' Left
                UndrawZoomBox
                boxX = ClampInt(boxX - moveStep, 0, MANDEL_W - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 131 ' Right
                UndrawZoomBox
                boxX = ClampInt(boxX + moveStep, 0, MANDEL_W - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 13  ' Enter: zoom
                UndrawZoomBox
                PushViewport
                pixLeft = boxX
                pixRight = boxX + boxSize - 1
                pixTop = boxY - MANDEL_TOP
                pixBottom = pixTop + boxSize - 1
                newXMin = xMin + currDX * pixLeft
                newXMax = xMin + currDX * pixRight
                newYMin = yMin + currDY * pixTop
                newYMax = yMin + currDY * pixBottom
                xMin = newXMin : xMax = newXMax
                yMin = newYMin : yMax = newYMax
                done = 1
            Case 27  ' Esc: cancel
                UndrawZoomBox
                done = 1
        End Select
    Loop
    DrawMenu
    RenderViewport
End Sub

For g = 0 To 255
    If g <= 63 Then
        rc% = 0 : gc% = g * 4 : bc% = 255
    ElseIf g <= 127 Then
        rc% = 0 : gc% = 255 : bc% = 255 - 4 * (g - 64)
    ElseIf g <= 191 Then
        rc% = 4 * (g - 128) : gc% = 255 : bc% = 0
    Else
        rc% = 255 : gc% = 255 - 4 * (g - 192) : bc% = 4 * (g - 192)
    EndIf
    colors%(g) = RGB(rc%, gc%, bc%)
Next g

CLS
ResetViewport
DrawMenu
RenderViewport

running = 1
Do While running = 1
    ' If a key arrived mid-render, use it first; otherwise block for a key.
    If pendingKey% <> -1 Then
        keyCode = pendingKey%
        pendingKey% = -1
    Else
        keyCode = GetKey()
    EndIf

    Select Case keyCode
        Case 90, 122          ' Z/z
            DoZoom
        Case 79, 111          ' O/o (zoom out/back)
            If PopViewport() Then
                DrawMenu
                RenderViewport
            EndIf
        Case 82, 114          ' R/r
            ResetViewport
            DrawMenu
            RenderViewport
        Case 27               ' Esc
            running = 0
    End Select
Loop

' SAVE IMAGE uses StartX, StartY, width, height
Save Image "out.bmp", 0, 0, SCREEN_W, SCREEN_H
