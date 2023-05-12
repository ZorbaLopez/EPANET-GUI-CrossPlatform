EPANET GUI Cross-Platform
======

## DESCRIPTION

**EPANET** is an industry-standard program for modeling the hydraulic and water quality behavior of water distribution system pipe networks. EPANET and its toolkit were originally developed by the U.S. Environmental Protection Agency (US EPA) using Delphi, a Windows closed source RAD-IDE developed by Borland and actually property of Embarcadero Technologies. [Read more about EPANET on Wikipedia](https://en.wikipedia.org/wiki/EPANET). 

(Please note that this project covers only the EPANET the graphical user interface, not the hydraulic and water quality solver engine.)

Due to the popularity of EPANET and its importance in the water industry, porting the program to other platforms is usually requested by users. The toolkit is in continuous development and succesfully ported by the **Open Water Analytics** (OWA) Community, and the repository can be found [here](https://github.com/OpenWaterAnalytics/EPANET/). However, due to Delphi is a Windows's exclusive RAD, the Graphical User Interface (GUI) has been ported using emulators like Wine.

The target of this project is to provide a true cross-platform implementation of the EPANET GUI using open source tools. **Lazarus** is an open source Pascal RAD-IDE, which facilitates the porting of the original Delphi code. Since MacOS and Linux don't support MDI forms some small adjustments to the interface are required. The result is a cross-platform MDI-free GUI for Windows, MacOS and Linux.

## CHANGES TO THE ORIGINAL EPANET GUI

Besides the conversion to a MDI-free GUI, other changes to the original GUI have been implemented. Even though the efforts of the Community, Lazarus is not a product as "polished" as Delphi and some features are not available. The most relevant are:
1. Lazarus's TAChart is not as powerful as Steema's TeeChart. Some of the most fancy characteristics of EPANET's Graphs are disabled, like 3D effects (or transparency in MacOS and Linux).
2. The Windows Metafile support in MacOS and Linux is limited. Metafiles can be loaded as background maps (somehow ugly) but Maps and Graphs cannot be exported as Metafiles in these platforms. Scalable Vector Graphics (SVG) format for exporting has been implemented instead, but without the Map's background image export feature.
3. Multilanguage support has been implemented. The original GUI is written in English language and versions of the old GUI in other languages are available in Internet. The code has been modified to use POT files, which allows multilanguage support. The Spanish translation is included in this repository, and can be activated in the `File (or Epanet Application Menu in MacOS) -> Preferences` menu item. The Spanish translation is mostly based on the excellent work of **Fernando Martínez Alzamora** and his team in the **Instituto Universitario de Investigación de Ingeniería del Agua y Medio Ambiente** (IIAMA), which can be found [here](https://www.iiama.upv.es/iiama/es/transferencia/software/epanet-esp.html).  


## INSTALLATION

Instructions for building the EPANET GUI from the source files can be found [here](https://github.com/ZorbaLopez/EPANET-GUI-CrossPlatform/blob/main/BUILDING.md).

## USAGE

See the help files included in the AppBundle in this repository, or refer to the official [Epanet Help](https://epanet22.readthedocs.io/en/latest/).

## CONTRIBUTING

Everyone is welcome to contribute to this project. Whether you are helping to resolve issues, reporting a new issue that hasn't yet been discovered, suggesting a new feature that would benefit your workflow, I value your time and effort. The path for contribution starts with the [Issues](https://github.com/ZorbaLopez/EPANET-GUI-CrossPlatform/issues). Look around at open Issues and the conversation around them, get engaged by commenting on an outstanding Issue or creating a new one.


## DISCLAIMER
This port is not affiliated with, nor endorsed by US EPA or OWA. For the last "official" release of EPANET please go to the [USEPA website](http://www2.epa.gov/water-research/epanet). This project is just a consequence of the curfew during the COVID-19 pandemic. 

This port has been tested with Lazarus 2.2.6 and FPC 3.2.2 in the below combinations of OS - Architecture. Other combinations avaliable in Lazarus should work, but may require code modifications or disable some features.
- Windows 10 22H2 - Intel 64 bits
- MacOS "Ventura" (13.3.1) - Intel 64 bits
- Linux Mint "Vera" (21.1) - Intel 64 bits GTK2 and QT5


