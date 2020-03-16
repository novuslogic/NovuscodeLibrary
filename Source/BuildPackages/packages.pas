Uses CodeImatic, 
     Delphi;


procedure BuldDelphiPackages(aDelphiVersion:  TDelphiVersion; aPackageName: string);
Var
  lsPackageNamedpk: string;
  lsPackageNamedproj: string;
  lVariableCmdLineList: TVariableCmdLineList;
  lVariableCmdLine: TVariableCmdLine;
begin
  //package.dpk
  lsPackageNamedpk := aPackageName + '.dpk';
  //package.dproj
  lsPackageNamedproj := aPackageName + '.dproj';


  try
    lVariableCmdLineList:= TVariableCmdLineList.Create;


    lVariableCmdLine:= TVariableCmdLine.Create;
    lVariableCmdLine.VariableName := 'DELPHIVER';
    lVariableCmdLine.Value := GetDelphiCompilerVersion(aDelphiVersion);
    lVariableCmdLineList.Add(lVariableCmdLine);

    lVariableCmdLine:= TVariableCmdLine.Create;
    lVariableCmdLine.VariableName := 'LIBSUFFIX';
    lVariableCmdLine.Value := GetDelphiPackageVersion(aDelphiVersion);
    lVariableCmdLineList.Add(lVariableCmdLine);

    lVariableCmdLine:= TVariableCmdLine.Create;
    lVariableCmdLine.VariableName := 'PACKAGENAMEDPK';
    lVariableCmdLine.Value := lsPackageNamedpk;
    lVariableCmdLineList.Add(lVariableCmdLine);


    lVariableCmdLine:= TVariableCmdLine.Create;
    lVariableCmdLine.VariableName := 'PACKAGENAMEDPROJ';
    lVariableCmdLine.Value := lsPackageNamedproj;
    lVariableCmdLineList.Add(lVariableCmdLine);

    if codegenex('packages.ccproject', lVariableCmdLineList, wd, '', false, '') <> 0 then
      RaiseException(erCustomError, 'failed.');  
  finally
    lVariableCmdLineList.Free;
  end;
  
  
 
end;


procedure BuildDelphiXEPackages;
begin
  Output.log('Delphi XE Packages ...');

  BuldDelphiPackages(DELPHIXE, 'package');
end;  


procedure BuildDelphiXE2Packages;
begin
  Output.log('Delphi XE2 Packages ...');

  BuldDelphiPackages(DELPHIXE2, 'package');
end;  

procedure BuildDelphiXE3Packages;
begin
  Output.log('Delphi XE3 Packages ...');

  BuldDelphiPackages(DELPHIXE3, 'package');
end;  

procedure BuildDelphiXE4Packages;
begin
  Output.log('Delphi XE4 Packages ...');

  BuldDelphiPackages(DELPHIXE4, 'package');
end;  
  
procedure BuildDelphiXE5Packages;
begin
  Output.log('Delphi XE5 Packages ...');

  BuldDelphiPackages(DELPHIXE5, 'package');
end;  

procedure BuildDelphiXE6Packages;
begin
  Output.log('Delphi XE6 Packages ...');

  BuldDelphiPackages(DELPHIXE6, 'package');
end;

procedure BuildDelphiXE7Packages;
begin
  Output.log('Delphi XE7 Packages ...');

  BuldDelphiPackages(DELPHIXE7, 'package');
end;

procedure BuildDelphiXE8Packages;
begin
  Output.log('Delphi XE8 Packages ...');

  BuldDelphiPackages(DELPHIXE8, 'package');
end;

procedure BuildDelphi10Packages;
begin
  Output.log('Delphi 10 Packages ...');

  BuldDelphiPackages(DELPHI10, 'package');
end;

procedure BuildDelphi10_1Packages;
begin
  Output.log('Delphi 10.1 Packages ...');

  BuldDelphiPackages(DELPHI10_1, 'package');
end;

procedure BuildDelphi10_2Packages;
begin
  Output.log('Delphi 10.2 Packages ...');

  BuldDelphiPackages(DELPHI10_2, 'package');
end;

procedure BuildDelphi10_3Packages;
begin
  Output.log('Delphi 10.3 Packages ...');

  BuldDelphiPackages(DELPHI10_3, 'package');
end;


begin
  Output.log('Building Delphi Packages ...');

  Output.log('Working Directory: '+wd);

   with Task.AddTask('BuildDelphiXEPackages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;

   with Task.AddTask('BuildDelphiXE2Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

   with Task.AddTask('BuildDelphiXE3Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;    

   with Task.AddTask('BuildDelphiXE4Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

  with Task.AddTask('BuildDelphiXE5Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  
 
  with Task.AddTask('BuildDelphiXE6Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

  with Task.AddTask('BuildDelphiXE7Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

   with Task.AddTask('BuildDelphiXE8Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

  with Task.AddTask('BuildDelphi10Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;  

  with Task.AddTask('BuildDelphi10_1Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end; 

   with Task.AddTask('BuildDelphi10_2Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end;

  with Task.AddTask('BuildDelphi10_3Packages') do
    begin
      Criteria.Failed.Abort := True;
     
      
    end; 

  if not Task.RunTargets(['BuildDelphiXEPackages',
        'BuildDelphiXE2Packages', 
        'BuildDelphiXE3Packages', 
        'BuildDelphiXE4Packages',
        'BuildDelphiXE5Packages',
        'BuildDelphiXE6Packages',
        'BuildDelphiXE7Packages',
        'BuildDelphiXE8Packages',
        'BuildDelphi10Packages',
        'BuildDelphi10_1Packages',
        'BuildDelphi10_2Packages',
        'BuildDelphi10_3Packages'
        ]) then 
    RaiseException(erCustomError, 'Failed RunTargets'); 


  
end.
