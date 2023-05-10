BUILDING
======

First of all ... **Clone this repo!!!**

## INSTALL THE LAZARUS RAD-IDE

- Follow the [Installing Lazarus](https://wiki.freepascal.org/Installing_Lazarus) instructions and install the dependencies required for your Operating System.
- Download the installer from the [Lazarus Downloads](https://www.lazarus-ide.org/index.php?page=downloads) page. Lazarus 2.2.6 with FPC 3.2.2 has been used for this project.
- Install the FPC compiler and sources.
- Install the Lazarus IDE.
- Check the installation compiling any test project in the `components` subfolder of Lazarus folder (in Linux Mint Lazarus, folder is located at `/usr/share/Lazarus/`).

## INSTALL THE PROJECT'S PACKAGE DEPENDENCIES
The project depends on the below packages, to be installed upfront:
- Printer4Lazarus
- TAChartLazarusPkg
- TAChartWMF
- TAChartFPVectorial (MacOS and Linux)
- fpvectorialpkg (MacOS and Linux)
- lhelpcontrolpkg (Linux)
- BGRABitmapPack (MacOS and Linux)

To install the packages:
- Open the Lazarus IDE and go to `Package -> Install/Uninstall Packages ...`
- Select the ones not installed (right panel) and click on `Install selection`
- Click on `Save and rebuild IDE`
- If any package is not in the list, like the BGRABitmapPack, go to `Package -> Online Package Manager ...`, select the packages required and click on `Install`

## INSTALL THE EPA PACKAGE
Although the EPA controls are integrated into the project, and there are no dependecies,  some of them are required at design time. Therefore, the EPA controls package must be installed in the Lazarus IDE.
- Open the EPA package in the Lazarus IDE by clicking on `Package -> Open Package File (.lpk)...`
- Navigate to the project's file `./components/epa.lpk` and click `open`
- On the EPA's Package dialog click on the dropdown list `Use` and click on `Install`
- Save and rebuild the IDE if prompted to do it

## GET THE EPANET TOOLKIT BINARY
The EPANET GUI will not work without the associated Toolkit library. Pre-compiled binaries are available at [OWA's repo releases](https://github.com/OpenWaterAnalytics/EPANET/releases). Version 2.2 of the Toolkit is used for this project.

If there is no binary for your OS-Architecture you can download the source code and compile by yourself, following the [OWA's build instructions](https://github.com/OpenWaterAnalytics/EPANET/blob/dev/BUILDING.md). I found Qt Creator very straightforward for this task; its integration with Cmake is awesome. 

The Toolkit binary must be saved in the folder `./API Engine/$(TargetCPU)-$(TargetOS)/`.

## COMPILE THE EXECUTABLE
To compile the EPANET GUI executable:
- Open the project in the Lazarus IDE by clicking on the menu item `Project -> Open Project ...`
- Navigate to the project's file `./Epanet/Epanet.lpi` and click `open` 
- Select the right Release build mode from the icon `Change Build Mode` (gear and wrench icon on the top-left side of the IDE)
- Click on `Run -> Build`. The executable will be saved in the folder `./Epanet/Release/$(TargetCPU)-$(TargetOS)/`

### Compilation Tips
The project is configured for Debug and Release versions for the three OS versions on Intel 64bit processor. Depending on your computer some changes can be made to compile in other combinations. Windows 32bit version is not critical; there are versions of Lazarus for both. Compiling the executable for older MacOS versions or Linux with the Qt widgetset will require to change the project options for the selected build mode. To open the project compilation options:
- Click on the menu item `Project -> Project Options ...` 
- Click on `Compiler Options` and select the correct build mode from the dropdown list
#### MacOS
- The current setting is for minimum MacOS "Big Sur" (11). If you want to compile for older versions ensure your SDK allows for it by executing in terminal the command `xcrun --show-sdk-version`. The minimum version can be changed in the project options. Go to the `Custom Options` and change the `-WM11.0` option by `-WM{yourSDKversion}`
- If your Mac has the M1 processor go to the `Config and Target` and change the `Target CPU family (-P)` to `aarch64`
#### Linux
- If you want to use the Qt5 widgetset go to `Additions and Overrides` and change the `LCLWidgetType` from `gtk2` to `qt5`. This will require to ensure you have the Qt5 widgetset installed and the pascal bindings. Follow the instructions [here](https://wiki.lazarus.freepascal.org/Qt5_Interface). The program requires the package libqt5pas to run. In Ubuntu-based distros you can install the package by executing in terminal/shell the command `sudo apt install libqt5pas1`.

## PREPARE THE APP BUNDLE

For the compiled binary work as expected all the executables must be placed in the right location on an app bundle. You can find these bundles in this repo, folder `AppBundle`. The binaries have been replaced by alias to assist you on the rigth place to copy each binary.
### Windows
Place the compiled GUI executable and the Toolkit binary into the bundle's root.
### MacOS
Place the compiled GUI executable in the folder `Epanet.app/Contents/MacOS/` and the Toolkit binary the folder `Epanet.app/Contents/Frameworks/`.
### Linux
Place the compiled GUI executable and the Toolkit binary into the bundle's root. In addition, Lazaru's lhelp excutable must be copied to the root directory. The lhelp can be found inside the Lazarus folder, in the subfolder `/components/chmhelp/lhep/`

# ENJOY!!!

In MacOS, place the app bundle in the Applications folder. In other OS's there is no such restriction. 