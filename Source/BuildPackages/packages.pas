Uses CodeImatic, 
     Delphi;


procedure BuldDelphiPackages(aDelphiVersion:  TDelphiVersion; aPackageName: string);
Var
  lsPackageNamedpk: string;
  lsPackageNamedproj: string;
begin
  //package.dpk
  lsPackageNamedpk := aPackageName + '.dpk';
  //package.dproj
  lsPackageNamedproj := aPackageName + '.dproj';
  
  if codegen('packages.ccproject', Format('DELPHIVER="%s";LIBSUFFIX="%s";PACKAGENAMEDPK="%s";PACKAGENAMEDPROJ="%s"', [GetDelphiCompilerVersion(aDelphiVersion),
          GetDelphiPackageVersion(aDelphiVersion), lsPackageNamedpk, lsPackageNamedproj]), wd, '') <> 0 then
     RaiseException(erCustomError, 'failed.');  
 
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
