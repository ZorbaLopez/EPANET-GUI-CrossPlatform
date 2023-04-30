EPANET GUI 2.2 Lazarus
======

## DESCRIPTION

**EPANET** is an industry-standard program for modeling the hydraulic and water quality behavior of water distribution system pipe networks. EPANET and its toolkit were originally developed by the U.S. Environmental Protection Agency (USEPA) using Delphi, a Windows closed source RAD-IDE developed by Borland and actually property of Embarcadero Technologies. [Read more about EPANET on Wikipedia](https://en.wikipedia.org/wiki/EPANET). 

(Please note that this project covers only the EPANET the graphical user interface, not the hydraulic and water quality solver engine.)

Due to the popularity of EPANET and its importance in the water industry, porting the program to other platforms is usually requested by users. The toolkit is in continuous development and succesfully ported by the **Open Water Analytics** (OWA) Community, and the repository can be found [here](https://github.com/OpenWaterAnalytics/EPANET/). However, due to Delphi is a Windows's exclusive RAD, the Graphical User Interface (GUI) has been ported using emulators like Wine.

The target of this project is to provide a true cross-platform implementation of the EPANET GUI using open source tools. **Lazarus** is an open source Pascal RAD-IDE, which facilitates the porting of the original Delphi code. Since MacOS and Linux don't support MDI forms some small adjustments to the interface are required. The result is a cross-platform MDI-free GUI for Windows, MacOS and Linux.

## CHANGES TO THE ORIGINAL EPANET GUI

Besides the conversion to a MDI-free GUI, other changes to the original GUI have been implemented. Although the efforts of the Community Lazarus is not a product as "polished" as Delphi. Some features are not available due to are not implemented yet. The most relevant are:
1. Lazaru's TAChart is not as powerful as Steema's TeeChart. Some of the most fancy characteristics of EPANET's Graphs are disabled, like 3D effects or Legend transparency.
2. The Windows Metafile support in MacOS and Linux is limited. Metafiles can be loaded as background maps (somehow ugly) but Maps and Graphs can not be exported as Metafiles in these platforms. Scalable Vector Graphics (SVG) format for exporting has been implemented instead, but without the Map's background image export feature.
3. Multilanguage support has been implemented. The original GUI is written in English language and versions of the old GUI in other languages are available in Internet. The code has been modified to use POT files, which allows multilanguage support. The Spanish translation is included in this repository, and can be activated in the `File -> Preferences` menu item. The Spanish translation is mostly based on the excellent work of **Fernando Martínez Alzamora** and his team in the **Instituto Universitario de Investigación de Ingeniería del Agua y Medio Ambiente** (IIAMA), which can be found [here](https://www.iiama.upv.es/iiama/es/transferencia/software/epanet-esp.html).  


## INSTALLATION

Instructions for building the EPANET GUI from the source files in this repository can be found [here](https://github.com/OpenWaterAnalytics/EPANET/blob/master/BUILDING.md).

## USAGE

See the [Help](http://wateranalytics.org/EPANET/) folder in this repository.

## CONTRIBUTING

Everyone is welcome to participate in this project. Whether you are helping to resolve issues, reporting a new issue that hasn't yet been discovered, suggesting a new feature that would benefit your workflow, or writing code (or tests, or scripts, or ...), I value your time and effort. The path for contribution starts with the [Issues](https://github.com/OpenWaterAnalytics/EPANET/issues). Look around at open Issues and the conversation around them, get engaged by commenting on an outstanding Issue or creating a new one. If you want to contribute code, it helps to give time to discuss the ideas you present and offer constructive feedback. Once you get a clear path forward, Fork this repo to your own account. Make your commits on your dev branch (or one based on dev). Once you are finished, you can open a Pull Request to test the code and discuss merging your changes back into the repository.


## DISCLAIMER
This port is not affiliated with, nor endorsed by USEPA or OWA. For the last "official" release of EPANET please go to the [USEPA website](http://www2.epa.gov/water-research/epanet). This project is just a consequence of the curfew during the COVID-19 pandemic. 

This port has been tested with Lazarus 2.2.6 and FPC 3.2.2 in the below combinations of OS - Architecture. Other combinations avaliable in Lazarus should work, but may require code modifications or disable some features.
- Windows 10 22H2 - Intel 64 bits
- MacOS "Ventura" (13.3.1) - Intel 64 bits
- Linux Mint "Vera" (21.1) - Intel 64 bits


