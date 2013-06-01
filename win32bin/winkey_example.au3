#include <GUIConstants.au3>
#include <WindowsConstants.au3>
#include <WinAPI.au3>
#include <Constants.au3>
#include <Misc.au3>

If _Singleton("comfort_keyboard", 1) = 0 Then
	MsgBox(0, "Warning", "already running")
	Exit
EndIf

$userDll = DllOpen("user32.dll")

Opt("SendAttachMode", 0)
Opt("SendKeyDelay", 50)
Opt("SendKeyDownDelay", 50)

;; Here's an awesome list of virtual key codes -> http://delphi.about.com/od/objectpascalide/l/blvkc.htm
$MOD_ALT = 0x0001
$MOD_CONTROL = 0x0002
$MOD_SHIFT = 0x0004
$MOD_WIN = 0x0008

$VK_0 = 0x30
$VK_1 = 0x31
$VK_2 = 0x32
$VK_3 = 0x33
$VK_4 = 0x34
$VK_5 = 0x35
$VK_6 = 0x36
$VK_7 = 0x37
$VK_8 = 0x38
$VK_9 = 0x39

$VK_A = 0x41
$VK_B = 0x42
$VK_C = 0x43
$VK_D = 0x44
$VK_E = 0x45
$VK_F = 0x46
$VK_G = 0x47
$VK_H = 0x48
$VK_I = 0x49
$VK_J = 0x4A
$VK_K = 0x4B
$VK_L = 0x4C
$VK_M = 0x4D
$VK_N = 0x4E
$VK_O = 0x4F
$VK_P = 0x50
$VK_Q = 0x51
$VK_R = 0x52
$VK_S = 0x53
$VK_T = 0x54
$VK_U = 0x55
$VK_V = 0x56
$VK_W = 0x57
$VK_X = 0x58
$VK_Y = 0x59
$VK_Z = 0x5A

$VK_NUMPAD0 = 0x60
$VK_NUMPAD1 = 0x61
$VK_NUMPAD2 = 0x62
$VK_NUMPAD3 = 0x63
$VK_NUMPAD4 = 0x64
$VK_NUMPAD5 = 0x65
$VK_NUMPAD6 = 0x66
$VK_NUMPAD7 = 0x67
$VK_NUMPAD8 = 0x68
$VK_NUMPAD9 = 0x69
$VK_SEPARATOR = 0x6C
$VK_SUBSTRUCT = 0x6D
$VK_DECIMAL = 0x6E
$VK_DIVIDE = 0x6F

$VK_F1 = 0x70
$VK_F2 = 0x71
$VK_F3 = 0x72
$VK_F4 = 0x73
$VK_F5 = 0x74
$VK_F6 = 0x75
$VK_F7 = 0x76
$VK_F8 = 0x77
$VK_F9 = 0x78
$VK_F10 = 0x79
$VK_F11 = 0x7A
$VK_F12 = 0x7B
$VK_F13 = 0x7C
$VK_F14 = 0x7D
$VK_F15 = 0x7E
$VK_F16 = 0x7F
$VK_F17 = 0x80
$VK_F18 = 0x81
$VK_F19 = 0x82
$VK_F20 = 0x83
$VK_F21 = 0x84
$VK_F22 = 0x85
$VK_F23 = 0x86
$VK_F24 = 0x87

$VK_LWIN = 0x5B
$VK_RWIN = 0x5C

$VK_ESCAPE = 0x1B
$VK_DELETE = 0x2E
$VK_INSERT = 0x2D
$VK_SHIFT = 0x10
$VK_CONTROL = 0xA2
$VK_MENU = 0x12
$VK_TAB = 0x09


Global $hWnd = GUICreate("Nothing", 100, 100)
;GUIRegisterMsg(0x0312, "MY_WM_HOTKEY")
;GUIRegisterMsg($WM_KEYDOWN, "WM_KEYDOWN")
;DllCall($userDll, "short", "RegisterHotKey", "hwnd", $hWnd, "int", "1354" , "uint", 0, "uint", $VK_LWIN)

Global $show = 0

If $show Then HotKeySet("{ESC}", "OnAutoItExit")

;HotKeySet("^t", "RootMap")

Opt("SendKeyDownDelay", 0)
Opt("SendKeyDelay", 0)


;; keypad codes
Global $kp_ignore[15]

$kp_ignore[0] = 71
$kp_ignore[1] = 72
$kp_ignore[2] = 73
$kp_ignore[3] = 111
$kp_ignore[4] = 75
$kp_ignore[5] = 76
$kp_ignore[6] = 77
$kp_ignore[7] = 106
$kp_ignore[8] = 79
$kp_ignore[9] = 80
$kp_ignore[10] = 81
$kp_ignore[11] = 109
$kp_ignore[12] = 82
$kp_ignore[13] = 83
$kp_ignore[14] = 78


Func isKpIgnore($scan)
	Local $c
	For $c In $kp_ignore
		If $c = $scan Then Return True
	Next
	Return False
EndFunc   ;==>isKpIgnore

Global $win_translate[256]

For $i = 0 To 255
	$win_translate[$i] = 0
Next

Func translateWinKey($scan, ByRef $newScan, ByRef $newVk)
	If $scan >= 0 And $scan <= 255 Then
		Local $c = $win_translate[$scan]
		If $c > 0 Then
			$newScan = BitAND($c, 0xFFFF)
			$newVk = BitShift($c, 16)
			Return True
		EndIf
	EndIf
	Return False
EndFunc   ;==>translateWinKey

;; loword is scan code, hiword is virtual key code
Func MakeTrans($scan, $vk)
	Return BitOR($scan, BitShift($vk, -16))
EndFunc   ;==>MakeTrans

$win_translate[21] = MakeTrans(82, $VK_INSERT) ;; Win-Y => Insert
$win_translate[35] = MakeTrans(83, $VK_DELETE) ;; Win-H => Delete
$win_translate[22] = MakeTrans(71, $VK_HOME) ;; Win-U => Home
$win_translate[36] = MakeTrans(79, $VK_END) ;; Win-J => End
$win_translate[23] = MakeTrans(73, $VK_PRIOR) ;; Win-I => PageUp
$win_translate[37] = MakeTrans(81, $VK_NEXT) ;; Win-K => PageDown
$win_translate[51] = MakeTrans(75, $VK_LEFT) ;; Win-< => Left
$win_translate[52] = MakeTrans(80, $VK_DOWN) ;; Win-> => Down
$win_translate[53] = MakeTrans(77, $VK_RIGHT) ;; Win-/ => Right
$win_translate[38] = MakeTrans(72, $VK_UP) ;; Win-L => Up


Global $lWinDown = False
Global $ctrlPressed = False
Global $altPressed = False
Global $sBuffer = ""
Global $hStub_KeyProc = DllCallbackRegister("_KeyProc", "int", "int;ptr;ptr")
Global $hMod = DllCall("kernel32.dll", "hwnd", "GetModuleHandle", "ptr", 0)
Global $hHook = DllCall($userDll, "hwnd", "SetWindowsHookEx", "int", _
		$WH_KEYBOARD_LL, "ptr", DllCallbackGetPtr($hStub_KeyProc), "hwnd", $hMod[0], "dword", 0)

While 1
	Sleep(10)
WEnd

Global $injecting = 0
Global $capture = 0
Global $captureUpKey = 0

Func StartCapture()
	Global $capture
	$capture = 1
EndFunc   ;==>StartCapture

Func EndCapture($upKey)
	Global $capture
	$capture = 0
	;;$captureUpKey = $upKey
EndFunc   ;==>EndCapture

Func Inject($vk, $isUp)
	Global $injecting
	Local $dwFlags = 0
	If $isUp Then $dwFlags = 2 ; KEYEVENTF_KEYUP
	$injecting = $injecting + 1
	DllCall($userDll, "dword", "keybd_event", "byte", $vk, "byte", 0, "dword", $dwFlags, "ptr", 0)
	$injecting = $injecting - 1
EndFunc   ;==>Inject

Func InjectSend($text)
	Global $injecting
	$injecting = $injecting + 1
	Send($text)
	$injecting = $injecting - 1
EndFunc   ;==>InjectSend

Func _KeyProc($nCode, $wParam, $lParam)
	Local $aRet, $KEYHOOKSTRUCT
	Global $injecting
	Global $lWinDown
	Global $show
	Global $capture
	Global $ctrlPressed
	Global $altPressed
	Global $captureUpKey

	If $nCode < 0 Or $injecting > 0 Then
		$aRet = DllCall($userDll, "long", "CallNextHookEx", "hwnd", $hHook[0], "int", $nCode, "ptr", $wParam, "ptr", $lParam)
		Return $aRet[0]
	EndIf

	If $wParam = $WM_KEYDOWN Or $wParam = $WM_KEYUP Then
		$KEYHOOKSTRUCT = DllStructCreate("dword;dword;dword;dword;ptr", $lParam)

		Local $vk = DllStructGetData($KEYHOOKSTRUCT, 1)
		Local $scan = DllStructGetData($KEYHOOKSTRUCT, 2)

		If $show And $wParam = $WM_KEYDOWN And $vk <> $VK_ESCAPE Then
			MsgBox(0, "", "Scan = " & $scan & " vk = " & $vk)
			Return 1
		EndIf

		If $capture = 2 Then
			If $wParam = $WM_KEYUP And $vk == $captureUpKey Then
				$capture = 0
				Return 1
			Else
				Return 1
			EndIf
		EndIf

		If $capture = 1 Then
			If $scan = 29 Or $scan = 42 Or $scan = 54 Then
				$aRet = DllCall($userDll, "long", "CallNextHookEx", "hwnd", $hHook[0], "int", $nCode, "ptr", $wParam, "ptr", $lParam)
				Return $aRet[0]
			EndIf

			If DoCapture($vk, $wParam = $WM_KEYUP) Then
				Return 1
			Else
				$aRet = DllCall($userDll, "long", "CallNextHookEx", "hwnd", $hHook[0], "int", $nCode, "ptr", $wParam, "ptr", $lParam)
				Return $aRet[0]
			EndIf
		EndIf

		If $wParam = $WM_KEYDOWN And $vk = $VK_T And _IsPressed("11", $userDll) Then
			RootMap()
			Return 1
		EndIf

		If isKpIgnore($scan) Then Return 1
		If $vk = $VK_LWIN Then
			$lWinDown = ($wParam = $WM_KEYDOWN)
			Return 1
		EndIf
		If $lWinDown Then
			Local $newScan = 0, $newVk = 0
			If translateWinKey($scan, $newScan, $newVk) Then
				;DllCall($dll,"int","keybd_event","int",$vkvalue,"int",0,"long",0,"long",0) ;To press a key
				;DllCall($dll,"int","keybd_event","int",$vkvalue,"int",0,"long",2,"long",0) ;To release a key
				Inject($newVk, $wParam = $WM_KEYUP)
				;MsgBox(0, "", "Simulating newScan=" & $newScan & " newVk=" & $newVk & " @error=" & @error)
				Return 1
			EndIf
		EndIf
	EndIf

	$aRet = DllCall($userDll, "long", "CallNextHookEx", "hwnd", $hHook[0], "int", $nCode, "ptr", $wParam, "ptr", $lParam)

	Return $aRet[0]
EndFunc   ;==>_KeyProc

Func OnAutoItExit()
	If $hStub_KeyProc Then DllCallbackFree($hStub_KeyProc)
	$hStub_KeyProc = 0

	DllCall($userDll, "int", "UnhookWindowsHookEx", "hwnd", $hHook[0])
	If @HotKeyPressed <> "" Then Exit
EndFunc   ;==>OnAutoItExit

Global $map = "root"

Func RootMap()
	Global $map

	$map = "root"
	StartCapture()
EndFunc   ;==>RootMap

While 1
	Sleep(100)
WEnd

Func ActivateFirefox()
	Local $ret = DllCall($userDll, "hwnd", "FindWindowA", "str", "MozillaWindowClass", "ptr", 0)
	Local $hWnd = $ret[0]
	If $hWnd Then
		Sleep(100)
		$ret = DllCall($userDll, "int", "SwitchToThisWindow", "hwnd", $hWnd, "bool", False)
		;;$ret = DllCall($userDll, "int", "SetForegroundWindow", "hwnd", $hWnd)
	EndIf
EndFunc   ;==>ActivateFirefox

Func ActivateEmacs()
	WinActivate("emacs")
EndFunc   ;==>ActivateEmacs

Func ActivatePrevWindow()
	InjectSend("!{TAB}")
EndFunc   ;==>ActivatePrevWindow

Func DoCapture($vk, $isUp)
	Global $map
	Local $ctrl = _IsPressed("11", $userDll)
	;MsgBox(0, "", "WM_KEYDOWN wParam=" & $vk & " ctrl=" & $ctrl)

	If $vk == $VK_ESCAPE or ($vk = $VK_G And $ctrl) Then
		EndCapture($vk)
		Return True
	ElseIf $map == "root" And $vk = $VK_T And Not $isUp Then
		EndCapture($vk)
		If Not $ctrl Then
			InjectSend("^t")
		Else
			ActivatePrevWindow()
		EndIf
		Return True
	ElseIf $map == "root" And $vk = $VK_B And Not $isUp Then
		ActivateFirefox()
		EndCapture($vk)
		Return True
	ElseIf $map == "root" And $vk = $VK_E And Not $isUp Then
		$map = "e"
		Return True
	ElseIf $map == "e" And $vk = $VK_E And Not $isUp Then
		ActivateEmacs()
		EndCapture($vk)
		Return True
	ElseIf $map == "root" And $vk = $VK_X And Not $isUp Then
		$map = "x"
		Return True
	ElseIf $map == "x" And $vk = $VK_X And Not $isUp Then
		Run("start-xterm.bat")
		EndCapture($vk)
		Return True
	Else
		Return True
	EndIf
EndFunc   ;==>DoCapture
