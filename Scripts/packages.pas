Uses Zcodegen, Delphi;

var 
  BuildDelphiXE4PackagesTask: tTask;


procedure BuildDelphiXE4Packages;
begin
  Output.log('Delphi XE4 Packages ...');


  if Zcodegen('packages.zcproject', 'packages.zcconfig', Format('DELPHIVER="%s";LIBSUFFIX="%s"', [GetDelphiCompilerVersion(DELPHIXE4), GetDelphiPackageVersion(DELPHIXE4)]), '') <> 0 then
     RaiseException(erCustomError, 'failed.'); 

end;  
  
begin
  Output.log('Building Delphi Packages ...');

  Output.log('Working Directory: '+wd);
  
  BuildDelphiXE4PackagesTask := Task.AddTask('BuildDelphiXE4Packages');
  if Not Assigned(BuildDelphiXE4PackagesTask) then
    RaiseException(erCustomError, 'not assigned BuildDelphiXE4PackagesTask'); 
  BuildDelphiXE4PackagesTask.Criteria.Failed.Abort := True;
 

  if not Task.RunTargets(['BuildDelphiXE4Packages']) then 
    RaiseException(erCustomError, 'missing procedure.'); 
   

  
end.
