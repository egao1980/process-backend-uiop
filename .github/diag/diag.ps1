param([string]$Sh)
$ErrorActionPreference = 'Continue'
Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;
public static class J {
  [DllImport("kernel32.dll", SetLastError=true, CharSet=CharSet.Unicode)] public static extern IntPtr CreateJobObjectW(IntPtr a, string n);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool AssignProcessToJobObject(IntPtr j, IntPtr p);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool TerminateJobObject(IntPtr j, uint c);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern IntPtr OpenProcess(uint a, bool i, int pid);
}
"@
New-Item -ItemType Directory -Force C:\t | Out-Null
function Tree($label) {
  Write-Host "---- $label"
  Get-CimInstance Win32_Process | Where-Object { $_.Name -match '^(sh|bash|sleep|dash)\.exe$' } |
    Sort-Object ProcessId | Format-Table -AutoSize ProcessId,ParentProcessId,Name,CommandLine | Out-String -Width 300 | Write-Host
}
function Alive($winpid) { [bool](Get-Process -Id $winpid -ErrorAction SilentlyContinue) }
Set-Content -Path C:\t\s.sh -Value 'sleep 100 & echo $! > /tmp/gcpid; cat /proc/$!/winpid > /tmp/gcwin; ps -W > /tmp/psw; sleep 100' -NoNewline
Set-Content -Path C:\t\s2.sh -Value 'sleep 1; sleep 100 & cat /proc/$!/winpid > /tmp/gcwin; sleep 100' -NoNewline

Write-Host "==== sh = $Sh"
$tmp = (& $Sh -c 'cygpath -w /tmp').Trim()
Write-Host "tmp=$tmp"
& $Sh -c 'uname -a'

foreach ($mode in 'taskkill','job','job-immediate') {
  Remove-Item -Force -ErrorAction SilentlyContinue "$tmp\gcwin","$tmp\gcpid","$tmp\psw"
  $script = if ($mode -eq 'job') { 'C:/t/s2.sh' } else { 'C:/t/s.sh' }
  $p = Start-Process -FilePath $Sh -ArgumentList $script -PassThru -NoNewWindow
  $job = [IntPtr]::Zero
  if ($mode -ne 'taskkill') {
    $job = [J]::CreateJobObjectW([IntPtr]::Zero, $null)
    $h = [J]::OpenProcess(0x1F0FFF, $false, $p.Id)
    $ok = [J]::AssignProcessToJobObject($job, $h)
    Write-Host "assign $mode ok=$ok err=$([Runtime.InteropServices.Marshal]::GetLastWin32Error())"
  }
  Start-Sleep -Seconds 3
  Write-Host "==== MODE $mode : shell winpid $($p.Id)"
  $gc = [int](Get-Content "$tmp\gcwin").Trim()
  Write-Host "gc posix pid: $(Get-Content "$tmp\gcpid" -ErrorAction SilentlyContinue)  gc winpid: $gc"
  Get-Content "$tmp\psw" -ErrorAction SilentlyContinue | Write-Host
  Tree "before ($mode)"
  if ($mode -eq 'taskkill') {
    & taskkill /T /F /PID $p.Id
  } else {
    $t = [J]::TerminateJobObject($job, 1)
    Write-Host "terminate ok=$t"
  }
  Start-Sleep -Seconds 3
  Tree "after ($mode)"
  Write-Host "RESULT mode=$mode gc winpid $gc alive after kill: $(Alive $gc)"
  Get-CimInstance Win32_Process | Where-Object { $_.Name -eq 'sleep.exe' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }
}
