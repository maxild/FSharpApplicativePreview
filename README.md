## How to work with F# 5 preview bits in Visual Studio

First the project settings should use the new lang version property set to `preview`

```xml
<PropertyGroup>
    <LangVersion>preview</LangVersion>
 </PropertyGroup>
 ```

To get the latest preview bits checkout `master` branch of `fsharp` repo, and then
build the project

```powershell
.\build.cmd -c Release -deployExtensions
```

At last from a "Visual Studio Command Prompt" run

```
devenv.exe /rootsuffix RoslynDev
```

This will start an experimental instance of Visual Studio outside a specific solution. The experimental instances are located at `C:\Users\<username>\AppData\Local\Microsoft\VisualStudio`.