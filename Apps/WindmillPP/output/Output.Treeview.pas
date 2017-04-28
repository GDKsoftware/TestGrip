unit Output.Treeview;

interface

uses
  Output.Interfaces,
  Vcl.ComCtrls;

type
  TOutputTreeview = class(TInterfacedObject, IOutput)
  private
    FTreeview: TTreeView;

    FErrorCount: Integer;
    FWarningCount: Integer;
    FInfoCount: Integer;

    function CreateFilenode(const Filepath: string): TTreeNode;
    function FindFilenode(const Filepath: string): TTreeNode;
    function FindOrCreateFilenode(const Filepath: string): TTreeNode;
  public
    constructor Create(const Tree: TTreeView);

    procedure SetWarningAsError(const Yes: Boolean);
    procedure Warning(const Filepath: string; const Linenumber: Integer; const Warning: string);
    procedure Error(const Filepath: string; const Linenumber: Integer; const Error: string);
    procedure Info(const Info: string);
    function WarningCount: Integer;
    function ErrorCount: Integer;
    function InfoCount: Integer;
  end;

  TOutputInfo = class
  public
    Text: string;
  end;

  TOutputSourcefile = class(TOutputInfo)
  public
    Filepath: string;
  end;

  TOutputMessage = class(TOutputInfo)
  public
    Linenumber: Integer;
  end;

  TOutputWarning = class(TOutputMessage);
  TOutputError = class(TOutputMessage);

implementation

uses
  System.SysUtils;

constructor TOutputTreeview.Create(const Tree: TTreeView);
begin
  inherited Create;

  FTreeview := Tree;
end;

function TOutputTreeview.FindFilenode(const Filepath: string): TTreeNode;
var
  IdxNode: Integer;
  Node: TTreeNode;
  Data: TOutputInfo;
begin
  Result := nil;

  for IdxNode := 0 to FTreeview.Items.Count - 1 do
  begin
    Node := FTreeview.Items[IdxNode];
    Data := TOutputInfo(Node.Data);
    if Data is TOutputSourcefile then
    begin
      if SameText(TOutputSourcefile(Data).Filepath, Filepath) then
      begin
        Result := Node;
        Exit;
      end;
    end;
  end;
end;

function TOutputTreeview.CreateFilenode(const Filepath: string): TTreeNode;
var
  Data: TOutputSourcefile;
begin
  Inc(FErrorCount);

  Data := TOutputSourcefile.Create;
  Data.Text := ExtractFileName(Filepath);
  Data.Filepath := Filepath;

  Result := FTreeview.Items.AddChild(nil, ExtractFileName(Filepath));
  Result.Data := Data;
end;

function TOutputTreeview.FindOrCreateFilenode(const Filepath: string): TTreeNode;
begin
  Result := FindFilenode(Filepath);
  if not Assigned(Result) then
  begin
    Result := CreateFilenode(Filepath);
  end;
end;

procedure TOutputTreeview.SetWarningAsError(const Yes: Boolean);
begin

end;

procedure TOutputTreeview.Warning(const Filepath: string; const Linenumber: Integer; const Warning: string);
var
  Data: TOutputWarning;
  Node: TTreeNode;
  Filenode: TTreeNode;
begin
  Inc(FWarningCount);

  Data := TOutputWarning.Create;
  Data.Text := Warning;
  Data.Linenumber := Linenumber;

  Filenode := FindOrCreateFilenode(Filepath);

  Node := FTreeview.Items.AddChild(Filenode, '[Warning] ' + Linenumber.ToString + ': ' + Warning);
  Node.Data := Data;
end;

procedure TOutputTreeview.Error(const Filepath: string; const Linenumber: Integer; const Error: string);
var
  Data: TOutputError;
  Node: TTreeNode;
  Filenode: TTreeNode;
begin
  Inc(FErrorCount);

  Data := TOutputError.Create;
  Data.Text := Error;
  Data.Linenumber := Linenumber;

  Filenode := FindOrCreateFilenode(Filepath);

  Node := FTreeview.Items.AddChild(Filenode, '[Error] ' + Linenumber.ToString + ': ' + Error);
  Node.Data := Data;
end;

procedure TOutputTreeview.Info(const Info: string);
var
  Data: TOutputInfo;
  Node: TTreeNode;
begin
  Inc(FInfoCount);

  Data := TOutputInfo.Create;
  Data.Text := Info;

  Node := FTreeview.Items.AddChild(nil, Info);
  Node.Data := Data;
end;

function TOutputTreeview.WarningCount: Integer;
begin
  Result := FWarningCount;
end;

function TOutputTreeview.ErrorCount: Integer;
begin
  Result := FErrorCount;
end;

function TOutputTreeview.InfoCount: Integer;
begin
  Result := FInfoCount;
end;

end.
