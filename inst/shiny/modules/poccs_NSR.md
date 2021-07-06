### **Module:**

**BACKGROUND**

The Native Status Resolver (NSR) is a data validation service which determines if a plant species (or other taxon such as genus or family) is native or introduced within the political division of observation. The NSR make this determination by comparison against country-, state- or county-level checklists.

**IMPLEMENTATION**

The R package `NSR` provides access to the NSR API.  It takes as input taxonomic (species, genus, family) and political division (country, state/province, and country/parish) information data, and returns information on the native status of those taxa in the given political divisions, as well as information on the basis of those decisions. This module also makes use of the Geographic Name Resolution Service (GNRS), as implemented in the R package `GNRS` in order to standardize political division names.

**REFERENCES**

https://github.com/ojalaquellueva/nsr
