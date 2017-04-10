Uses Zcodegen, Delphi;

var 
  BuildDelphiXE3PackagesTask: tTask;
  BuildDelphiXE4PackagesTask: tTask;


procedure BuldDelphiPackages(aCompilerVersion: integer);
begin
  if Zcodegen('packages.zcproject', 'packages.zcconfig', Format('DELPHIVER="%s";LIBSUFFIX="%s"', [GetDelphiCompilerVersion(aCompilerVersion), GetDelphiPackageVersion(aCompilerVersion)]), '') <> 0 then
     RaiseException(erCustomError, 'failed.');  
end;


procedure BuildDelphiXE3Packages;
begin
  Output.log('Delphi XE3 Packages ...');

  BuldDelphiPackages(DELPHIXE3);
end;  

procedure BuildDelphiXE4Packages;
begin
  Output.log('Delphi XE4 Packages ...');

  BuldDelphiPackages(DELPHIXE4);
end;  
  
begin
  Output.log('Building Delphi Packages ...');

  Output.log('Working Directory: '+wd);

  BuildDelphiXE3PackagesTask := Task.AddTask('BuildDelphiXE3Packages');
  if Not Assigned(BuildDelphiXE3PackagesTask) then
    RaiseException(erCustomError, 'not assigned BuildDelphiXE3PackagesTask'); 
  BuildDelphiXE3PackagesTask.Criteria.Failed.Abort := True;
 
  
  BuildDelphiXE4PackagesTask := Task.AddTask('BuildDelphiXE4Packages');
  if Not Assigned(BuildDelphiXE4PackagesTask) then
    RaiseException(erCustomError, 'not assigned BuildDelphiXE4PackagesTask'); 
  BuildDelphiXE4PackagesTask.Criteria.Failed.Abort := True;
 

  if not Task.RunTargets(['BuildDelphiXE3Packages', 'BuildDelphiXE4Packages']) then 
    RaiseException(erCustomError, 'missing procedure.'); 
   

  
end.
