VERSION 5.00
Object = "{6607E61F-2926-45E6-90FE-BEC12B6CF94E}#1.0#0"; "EASYCO~1.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomctl.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "Comdlg32.ocx"
Begin VB.Form FormMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "EasyControl Demo"
   ClientHeight    =   7710
   ClientLeft      =   30
   ClientTop       =   330
   ClientWidth     =   11535
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7710
   ScaleWidth      =   11535
   StartUpPosition =   3  'Windows Default
   Begin MSComctlLib.Slider Slider2 
      Height          =   2175
      Left            =   11040
      TabIndex        =   12
      Top             =   3240
      Width           =   375
      _ExtentX        =   661
      _ExtentY        =   3836
      _Version        =   393216
      Orientation     =   1
      Max             =   90
   End
   Begin MSComctlLib.Slider Slider1 
      Height          =   2175
      Left            =   10560
      TabIndex        =   11
      Top             =   3240
      Width           =   435
      _ExtentX        =   767
      _ExtentY        =   3836
      _Version        =   393216
      Orientation     =   1
      Max             =   360
   End
   Begin VB.Timer TimerGPS 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   11040
      Top             =   3000
   End
   Begin VB.CommandButton Command5 
      Caption         =   "GPS test"
      Height          =   375
      Left            =   10560
      TabIndex        =   10
      Top             =   2640
      Width           =   855
   End
   Begin VB.CommandButton Command4 
      Caption         =   "清除书签"
      Height          =   375
      Left            =   10560
      TabIndex        =   8
      Top             =   2040
      Width           =   855
   End
   Begin VB.CommandButton Command2 
      Caption         =   "下一个"
      Height          =   375
      Left            =   10560
      TabIndex        =   6
      Top             =   1680
      Width           =   855
   End
   Begin VB.TextBox TextBookmark 
      Height          =   285
      Left            =   10560
      TabIndex        =   9
      Text            =   "老鼠"
      Top             =   1080
      Width           =   855
   End
   Begin VB.CommandButton Command3 
      Caption         =   "上一个"
      Height          =   375
      Left            =   10560
      TabIndex        =   7
      Top             =   1320
      Width           =   855
   End
   Begin VB.CommandButton Command1 
      Caption         =   "增加书签"
      Height          =   375
      Left            =   10560
      TabIndex        =   5
      Top             =   720
      Width           =   855
   End
   Begin VB.ComboBox CBLayers 
      Height          =   315
      Left            =   10200
      Style           =   2  'Dropdown List
      TabIndex        =   4
      ToolTipText     =   "当前图层"
      Top             =   0
      Width           =   1215
   End
   Begin VB.TextBox TextOutput 
      Height          =   732
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   3
      Text            =   "FormMain.frx":0000
      Top             =   6840
      Width           =   10335
   End
   Begin MSComDlg.CommonDialog cdews 
      Left            =   120
      Top             =   360
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "ews"
      Filter          =   "EasyMap WorkSpace (*.ews)|*.ews|"
   End
   Begin MSComctlLib.Toolbar Toolbar1 
      Height          =   390
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   10215
      _ExtentX        =   18018
      _ExtentY        =   688
      ButtonWidth     =   609
      ButtonHeight    =   582
      Wrappable       =   0   'False
      Appearance      =   1
      ImageList       =   "ImageList1"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   30
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "New Workspace"
            Description     =   "New Workspace"
            Object.ToolTipText     =   "新建工作空间"
            ImageIndex      =   1
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Open Workspace"
            Description     =   "Open Workspace"
            Object.ToolTipText     =   "打开工作空间"
            ImageIndex      =   2
         EndProperty
         BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Save Workspace"
            Description     =   "Save Workspace"
            Object.ToolTipText     =   "保存工作空间"
            ImageIndex      =   3
         EndProperty
         BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Workspace SaveAs"
            Description     =   "Workspace SaveAs"
            Object.ToolTipText     =   "工作空间另存为"
            ImageIndex      =   4
         EndProperty
         BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button6 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Zoom In"
            Description     =   "Zoom In"
            Object.ToolTipText     =   "拉框放大"
            ImageIndex      =   5
         EndProperty
         BeginProperty Button7 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Zoom Out"
            Description     =   "Zoom Out"
            Object.ToolTipText     =   "拉框缩小"
            ImageIndex      =   6
         EndProperty
         BeginProperty Button8 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Zoom Pan"
            Description     =   "Zoom Pan"
            Object.ToolTipText     =   "视图漫游"
            ImageIndex      =   7
         EndProperty
         BeginProperty Button9 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Zoom All"
            Description     =   "Zoom All"
            Object.ToolTipText     =   "全图显示"
            ImageIndex      =   8
         EndProperty
         BeginProperty Button10 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Update View"
            Description     =   "Update View"
            Object.ToolTipText     =   "视图刷新"
            ImageIndex      =   9
         EndProperty
         BeginProperty Button11 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button12 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Load SlimData"
            Description     =   "Load SlimData"
            Object.ToolTipText     =   "加载EasyMap SlimData"
            ImageIndex      =   10
         EndProperty
         BeginProperty Button13 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Load Shapefile"
            Description     =   "Load Shapefile"
            Object.ToolTipText     =   "加载ArcGIS Shapefile"
            ImageIndex      =   11
         EndProperty
         BeginProperty Button14 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Load BMP"
            Description     =   "Load BMP"
            Object.ToolTipText     =   "加载已定位BMP"
            ImageIndex      =   12
         EndProperty
         BeginProperty Button15 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button16 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Identify"
            Description     =   "Identify"
            Object.ToolTipText     =   "要素查询"
            ImageIndex      =   24
         EndProperty
         BeginProperty Button17 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button18 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Undo"
            Description     =   "Undo"
            Object.ToolTipText     =   "撤销编辑"
            ImageIndex      =   13
         EndProperty
         BeginProperty Button19 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Redo"
            Description     =   "Redo"
            Object.ToolTipText     =   "重复编辑"
            ImageIndex      =   14
         EndProperty
         BeginProperty Button20 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "SaveEdit"
            Description     =   "SaveEdit"
            Object.ToolTipText     =   "保存编辑"
            ImageIndex      =   15
         EndProperty
         BeginProperty Button21 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "CancelEdit"
            Description     =   "CancelEdit"
            Object.ToolTipText     =   "放弃编辑"
            ImageIndex      =   16
         EndProperty
         BeginProperty Button22 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button23 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "NewFeature"
            Description     =   "NewFeature"
            Object.ToolTipText     =   "创建新要素"
            ImageIndex      =   17
         EndProperty
         BeginProperty Button24 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "SelectObjects"
            Description     =   "SelectObjects"
            Object.ToolTipText     =   "选择对象"
            ImageIndex      =   18
         EndProperty
         BeginProperty Button25 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "MoveObjects"
            Description     =   "MoveObjects"
            Object.ToolTipText     =   "移动所选择的对象"
            ImageIndex      =   19
         EndProperty
         BeginProperty Button26 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "DeleteObjects"
            Description     =   "DeleteObjects"
            Object.ToolTipText     =   "删除所选择的对象"
            ImageIndex      =   20
         EndProperty
         BeginProperty Button27 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "AddLineElement"
            Description     =   "AddLineElement"
            Object.ToolTipText     =   "增加线条"
            ImageIndex      =   21
         EndProperty
         BeginProperty Button28 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "AddEllipseElement"
            Description     =   "AddEllipseElement"
            Object.ToolTipText     =   "增加椭圆"
            ImageIndex      =   22
         EndProperty
         BeginProperty Button29 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button30 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "SetSymbol"
            Description     =   "SetSymbol"
            Object.ToolTipText     =   "设置符号"
            ImageIndex      =   23
         EndProperty
      EndProperty
   End
   Begin VB.Frame Frame1 
      Height          =   6255
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   10335
      Begin MSComDlg.CommonDialog cdfile 
         Left            =   2640
         Top             =   0
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
      End
      Begin MSComctlLib.ImageList ImageList1 
         Left            =   7680
         Top             =   0
         _ExtentX        =   979
         _ExtentY        =   979
         BackColor       =   -2147483643
         ImageWidth      =   16
         ImageHeight     =   16
         MaskColor       =   255
         _Version        =   393216
         BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
            NumListImages   =   24
            BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0008
               Key             =   ""
            EndProperty
            BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":011A
               Key             =   ""
            EndProperty
            BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":022C
               Key             =   ""
            EndProperty
            BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":033E
               Key             =   ""
            EndProperty
            BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0450
               Key             =   ""
            EndProperty
            BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0562
               Key             =   ""
            EndProperty
            BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0674
               Key             =   ""
            EndProperty
            BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0786
               Key             =   ""
            EndProperty
            BeginProperty ListImage9 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0898
               Key             =   ""
            EndProperty
            BeginProperty ListImage10 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":09AA
               Key             =   ""
            EndProperty
            BeginProperty ListImage11 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0ABC
               Key             =   ""
            EndProperty
            BeginProperty ListImage12 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0BCE
               Key             =   ""
            EndProperty
            BeginProperty ListImage13 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0CE0
               Key             =   ""
            EndProperty
            BeginProperty ListImage14 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0DF2
               Key             =   ""
            EndProperty
            BeginProperty ListImage15 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":0F04
               Key             =   ""
            EndProperty
            BeginProperty ListImage16 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":1016
               Key             =   ""
            EndProperty
            BeginProperty ListImage17 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":1128
               Key             =   ""
            EndProperty
            BeginProperty ListImage18 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":123A
               Key             =   ""
            EndProperty
            BeginProperty ListImage19 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":134C
               Key             =   ""
            EndProperty
            BeginProperty ListImage20 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":145E
               Key             =   ""
            EndProperty
            BeginProperty ListImage21 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":1570
               Key             =   ""
            EndProperty
            BeginProperty ListImage22 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":1682
               Key             =   ""
            EndProperty
            BeginProperty ListImage23 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":1804
               Key             =   ""
            EndProperty
            BeginProperty ListImage24 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "FormMain.frx":199E
               Key             =   ""
            EndProperty
         EndProperty
      End
      Begin EASYCONTROLLib.EasyControl ec 
         Height          =   5895
         Left            =   120
         TabIndex        =   2
         Top             =   240
         Width           =   10095
         _Version        =   65536
         _ExtentX        =   17806
         _ExtentY        =   10398
         _StockProps     =   0
      End
   End
   Begin VB.Label Label1 
      Caption         =   "旋转"
      Height          =   255
      Left            =   10800
      TabIndex        =   13
      Top             =   5400
      Width           =   495
   End
End
Attribute VB_Name = "FormMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim m_ews As String
Dim m_GPSLayer As EasyLayer

Public Sub NewWorkspace()
    ec.NewWorkspace
    ec.UpdateView
    RefreshLayerList
End Sub

Public Sub OpenWorkspace()
    cdews.FileName = ""
    cdews.ShowOpen
    If cdews.FileName = "" Then
          Exit Sub
    End If

    ec.NewWorkspace
    m_ews = ""
    If ec.LoadWorkspace(cdews.FileName) Then
        m_ews = cdews.FileName
    End If
    ec.UpdateView
    RefreshLayerList
End Sub

Public Sub SaveWorkspace()
    If m_ews = "" Then
        Exit Sub
    End If
    
    ec.SaveWorkspace m_ews
End Sub

Public Sub WorkspaceSaveAs()
    cdews.ShowOpen
    If cdews.FileName = "" Then
          Exit Sub
    End If

    If ec.SaveWorkspace(cdews.FileName) Then
        m_ews = cdews.FileName
    End If
End Sub

Public Sub ZoomIn()
    ec.SetActionZoomin
End Sub

Public Sub ZoomOut()
    ec.SetActionZoomout
End Sub

Public Sub ZoomPan()
    ec.SetActionPan
End Sub

Public Sub ZoomAll()
    ec.FullExtentView
End Sub

Public Sub UpdateView()
    ec.UpdateView
End Sub

Public Sub LoadSlimData()
    cdfile.DefaultExt = "esd"
    cdfile.Filter = "EasyMap SlimData (*.esd)|*.esd|"
    cdfile.ShowOpen
    If cdfile.FileName = "" Then
          Exit Sub
    End If

    If Not ec.LoadSlimData(cdfile.FileName, False) Then
        Exit Sub
    End If
    
    If ec.GetLayerCount = 1 Then
        ec.FullExtentView
        RefreshLayerList
    Else
        ec.UpdateView
    End If
End Sub

Public Sub LoadShapeFile()
    cdfile.DefaultExt = "shp"
    cdfile.Filter = "ArcGIS ShapeFile (*.shp)|*.shp|"
    cdfile.ShowOpen
    If cdfile.FileName = "" Then
          Exit Sub
    End If

    If Not ec.LoadShapeFile(cdfile.FileName, UNIT_M, 2000, 0.0001, 5, False) Then
        Exit Sub
    End If
    
    If ec.GetLayerCount = 1 Then
        ec.FullExtentView
        RefreshLayerList
    Else
        ec.UpdateView
    End If
End Sub

Public Sub LoadBMP()
    cdfile.DefaultExt = "bmp"
    cdfile.Filter = "带dom信息的bmp文件 (*.bmp)|*.bmp|"
    cdfile.ShowOpen
    If cdfile.FileName = "" Then
          Exit Sub
    End If

    If Not ec.LoadOrientBmp(cdfile.FileName) Then
        Exit Sub
    End If
    
    If ec.GetLayerCount = 1 Then
        ec.FullExtentView
        RefreshLayerList
    Else
        ec.UpdateView
    End If
End Sub

Public Sub Identify()
    ec.SetActionNone
    ec.ClearHighlight
    ec.RefreshWindow
End Sub

Public Sub Undo()
    ec.EditUndo
    ec.UpdateView
End Sub

Public Sub Redo()
    ec.EditRedo
    ec.UpdateView
End Sub

Public Sub SaveEdit()
    ec.EditSave
End Sub

Public Sub CancelEdit()
    ec.EditCancel
    ec.UpdateView
End Sub

Public Sub NewFeature()
    ec.SetActionNone
    If CBLayers.ListCount <= 0 Then
        MsgBox "没有加载图层"
        Exit Sub
    End If
    
    If CBLayers.ListIndex < 0 Then
        MsgBox "请选择用于添加要素的图层"
        Exit Sub
    End If

    Dim layer As EASYCONTROLLib.EasyLayer
    Set layer = ec.GetLayer(CBLayers.ListIndex)
    If Not ec.EditTrackFeature(layer, "Oops!") Then
        MsgBox "当前所选择的图层类型不正确或处于只读状态，请重新选择用于添加要素的图层"
        Exit Sub
    End If
End Sub

Public Sub SelectObjects()
    ec.SelectByEnvelope
End Sub

Public Sub MoveObjects()
    ec.MoveSelectedObjects
End Sub

Public Sub DeleteObjects()
    ec.DeleteSelectObjects
    ec.UpdateView
End Sub

Public Function GetElementLayer() As EASYCONTROLLib.EasyLayer
    Dim layer As EASYCONTROLLib.EasyLayer
    If CBLayers.ListIndex >= 0 Then
        Set layer = ec.GetLayer(CBLayers.ListIndex)
    End If

    Dim flag As Boolean
    flag = True
    If layer Is Nothing Then
        flag = False
    ElseIf layer.GetLayerType <> EASYLAYERTYPE_ELEMENTLAYER Then
        flag = False
    End If

    If Not flag Then
        Set layer = Nothing
        Dim layertmp As EASYCONTROLLib.EasyLayer
        Dim i As Long
        For i = 0 To ec.GetLayerCount - 1
            Set layertmp = ec.GetLayer(i)
            If layertmp.GetLayerType = EASYLAYERTYPE_ELEMENTLAYER Then
                Set layer = layertmp
                CBLayers.ListIndex = i
            End If
        Next i
    End If
    
    If layer Is Nothing Then
        ec.NewElementLayer "elements"
        Set layer = ec.GetLayer(0)
    End If

    RefreshLayerList
    CBLayers.ListIndex = 0

    Set GetElementLayer = layer
End Function

Public Sub AddLineElement()
    Dim layer As EASYCONTROLLib.EasyLayer
    Set layer = GetElementLayer
    ec.EditTrackFreehandLineElement layer
End Sub

Public Sub AddEllipseElement()
    Dim layer As EASYCONTROLLib.EasyLayer
    Set layer = GetElementLayer
    ec.EditTrackEllipseElement layer
End Sub

Public Sub SetSymbol()
    If CBLayers.ListIndex < 0 Then
        MsgBox "请选择一个要素层"
        Exit Sub
    End If
    
    Dim layer As EASYCONTROLLib.EasyLayer
    Set layer = ec.GetLayer(CBLayers.ListIndex)
    If (layer.GetLayerType <> EASYLAYERTYPE_SLIMLAYER) And (layer.GetLayerType <> EASYLAYERTYPE_SHAPELAYER) Then
        MsgBox "当前图层不是要素层，请选择一个要素层"
        Exit Sub
    End If
    
    layer.ShowRendererUI
    ec.UpdateView
End Sub

Private Sub CBLayers_Click()
    If ec.GetCurrentAction = ActionTrackFeature Then
        ec.SetActionNone
    End If
End Sub

Private Sub Command1_Click()
    ec.AddBookmark TextBookmark.Text
    ec.RefreshWindow
End Sub

Private Sub Command2_Click()
    ec.NextBookmark
    ec.UpdateView
End Sub

Private Sub Command3_Click()
    ec.PreviousBookmark
    ec.UpdateView
End Sub

Private Sub Command4_Click()
    ec.ClearBookmarks
    ec.RefreshWindow
End Sub

Private Sub Command5_Click()
    ec.ClearRapidDrawLayers
    Set m_GPSLayer = Nothing

    Set m_GPSLayer = ec.RapidDrawGPS

    Dim i As Long
    For i = 0 To 120
        Dim pFeature As EASYCONTROLLib.EasyFeature
        Set pFeature = m_GPSLayer.CreateFeature
    
        Dim x_dev As Long
        Dim y_dev As Long
        x_dev = Int(Rnd * ec.Width / 16)
        y_dev = Int(Rnd * ec.Height / 16)
        Dim x As Double
        Dim y As Double
        ec.Window2Map x_dev, y_dev, x, y
        Dim pPoint As New EASYCONTROLLib.EasyPoint
        pPoint.SetCoordinates x, y, 0
        pFeature.SetGeometry pPoint
        pFeature.Update
    Next i

    m_GPSLayer.SaveEdit
    
    TimerGPS.Enabled = True
End Sub

Private Sub ec_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If ec.GetCurrentAction = ActionDefault Then
        ec.ClearHighlight
        TextOutput.Text = ""

        Dim winx As Long
        Dim winy As Long
        winx = x / Screen.TwipsPerPixelX
        winy = y / Screen.TwipsPerPixelY
        Dim x1 As Double
        Dim y1 As Double
        Dim x2 As Double
        Dim y2 As Double
        ec.Window2Map winx - 5, winy + 5, x1, y1
        ec.Window2Map winx + 5, winy - 5, x2, y2
        
        Dim i As Long
        For i = 0 To ec.GetLayerCount - 1
            Dim layer As EASYCONTROLLib.EasyLayer
            Set layer = ec.GetLayer(i)
            If (layer.GetLayerType = EASYLAYERTYPE_SLIMLAYER) Or (layer.GetLayerType = EASYLAYERTYPE_SHAPELAYER) Then
                Dim intarray As EASYCONTROLLib.EasyIntArray
                Set intarray = layer.Identify(x1, y1, x2, y2, True)
                If Not intarray Is Nothing Then
                    If intarray.GetCount > 0 Then
                        TextOutput.Text = TextOutput.Text & "图层－" & layer.GetName & Chr(13)
                    
                        Dim j As Long
                        For j = 0 To intarray.GetCount - 1
                            Dim fid As Long
                            intarray.GetAt j, fid
                            TextOutput.Text = TextOutput.Text & " fid=" & fid & ", "
                            
                            '取出feature
                            Dim feature As EASYCONTROLLib.EasyFeature
                            Set feature = layer.GetFeature(fid)
                            ec.AddHighlight feature.GetGeometry
                            Dim k As Long
                            For k = 0 To feature.GetFieldCount
                                Dim fieldvalue As String
                                fieldvalue = feature.GetFieldValue(k)
                                '??
                            Next k
                        Next j
                        TextOutput.Text = TextOutput.Text & Chr(13)
                    End If
                End If
            End If
        Next i
    End If
    
    ec.RefreshWindow
End Sub

Private Sub ec_NewElementTracked(ByVal id As Long, setundopoint As Boolean)
    TextOutput.Text = "新element已经添加，id：" & id
End Sub

Private Sub ec_NewFeatureTracked(ByVal fid As Long, setundopoint As Boolean)
    TextOutput.Text = "新要素已经添加，fid：" & fid
End Sub

Private Sub Slider1_Change()
    ec.SetPlaneRotate Slider1.Value
    ec.UpdateView
End Sub

Private Sub Slider2_Change()
    ec.SetAttitude Slider2.Value
    ec.UpdateView
End Sub

Private Sub TimerGPS_Timer()
    Dim fids As EasyIntArray
    m_GPSLayer.SelectAll
    Set fids = m_GPSLayer.GetSelection
    
    Dim pFeature As EASYCONTROLLib.EasyFeature
    Dim fid As Long
    Dim x_dev As Long
    Dim y_dev As Long
    Dim x_min As Double
    Dim y_min As Double
    Dim x_max As Double
    Dim y_max As Double
    Dim x As Double
    Dim y As Double
    Dim z As Double
    Dim pPoint As EASYCONTROLLib.EasyPoint
    Dim step_max As Double
    Dim i As Long
    For i = 0 To fids.GetCount / 2 - 1
        fids.GetAt i, fid
        Set pFeature = m_GPSLayer.GetFeature(fid)

        x_dev = 0
        y_dev = 0
        ec.Window2Map x_dev, y_dev, x_min, y_max
        x_dev = ec.Width / 16
        y_dev = ec.Height / 16
        ec.Window2Map x_dev, y_dev, x_max, y_min

        Set pPoint = pFeature.GetGeometry
        pPoint.GetCoordinates x, y, z

        step_max = x_max - x_min
        step_max = step_max / 99.9

        x = x + Rnd * step_max
        y = y + Rnd * step_max

        If x > x_max Then
            x = x_min
        End If

        If y > y_max Then
            y = y_min
        End If

        m_GPSLayer.ModifyGPSPoint fid, x, y
    Next i
    
    For i = fids.GetCount / 2 To fids.GetCount - 1
        fids.GetAt i, fid
        Set pFeature = m_GPSLayer.GetFeature(fid)

        x_dev = 0
        y_dev = 0
        ec.Window2Map x_dev, y_dev, x_min, y_max
        x_dev = ec.Width / 16
        y_dev = ec.Height / 16
        ec.Window2Map x_dev, y_dev, x_max, y_min

        Set pPoint = pFeature.GetGeometry
        pPoint.GetCoordinates x, y, z

        step_max = x_max - x_min
        step_max = step_max / 99.9

        x = x + Rnd * step_max
        y = y - Rnd * step_max

        If x > x_max Then
            x = x_min
        End If

        If y < y_min Then
            y = y_max
        End If

        m_GPSLayer.ModifyGPSPoint fid, x, y
    Next i

    ec.RefreshWindow
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)

    If Button.Key = "New Workspace" Then
        NewWorkspace
        Exit Sub
    End If
    
    If Button.Key = "Open Workspace" Then
        OpenWorkspace
        Exit Sub
    End If
    
    If Button.Key = "Save Workspace" Then
        SaveWorkspace
        Exit Sub
    End If
    
    If Button.Key = "Workspace SaveAs" Then
        WorkspaceSaveAs
        Exit Sub
    End If
    
    If Button.Key = "Zoom In" Then
        ZoomIn
        Exit Sub
    End If
    
    If Button.Key = "Zoom Out" Then
        ZoomOut
        Exit Sub
    End If
    
    If Button.Key = "Zoom Pan" Then
        ZoomPan
        Exit Sub
    End If
    
    If Button.Key = "Zoom All" Then
        ZoomAll
        Exit Sub
    End If
    
    If Button.Key = "Update View" Then
        UpdateView
        Exit Sub
    End If
    
    If Button.Key = "Load SlimData" Then
        LoadSlimData
        Exit Sub
    End If
    
    If Button.Key = "Load Shapefile" Then
        LoadShapeFile
        Exit Sub
    End If
    
    If Button.Key = "Load BMP" Then
        LoadBMP
        Exit Sub
    End If
    
    If Button.Key = "Identify" Then
        Identify
        Exit Sub
    End If
    
    If Button.Key = "Undo" Then
        Undo
        Exit Sub
    End If
    
    If Button.Key = "Redo" Then
        Redo
        Exit Sub
    End If
    
    If Button.Key = "SaveEdit" Then
        SaveEdit
        Exit Sub
    End If
    
    If Button.Key = "CancelEdit" Then
        CancelEdit
        Exit Sub
    End If
    
    If Button.Key = "NewFeature" Then
        NewFeature
        Exit Sub
    End If
    
    If Button.Key = "SelectObjects" Then
        SelectObjects
        Exit Sub
    End If
    
    If Button.Key = "MoveObjects" Then
        MoveObjects
        Exit Sub
    End If
    
    If Button.Key = "DeleteObjects" Then
        DeleteObjects
        Exit Sub
    End If
    
    If Button.Key = "AddLineElement" Then
        AddLineElement
        Exit Sub
    End If
    
    If Button.Key = "AddEllipseElement" Then
        AddEllipseElement
        Exit Sub
    End If
    
    If Button.Key = "SetSymbol" Then
        SetSymbol
        Exit Sub
    End If
    
End Sub

Private Sub RefreshLayerList()
    CBLayers.Clear
    Dim i As Long
    Dim count As Long
    count = ec.GetLayerCount
    For i = 0 To count - 1
        Dim layer As EASYCONTROLLib.EasyLayer
        Set layer = ec.GetLayer(i)
        CBLayers.AddItem layer.GetName
    Next i
    
    If CBLayers.ListCount > 0 Then
        CBLayers.ListIndex = 0
    End If
End Sub



