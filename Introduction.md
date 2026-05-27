<style type="text/css">
<!--
 .tab { font-family: Arial;margin-left: 5px; }
-->
</style>

<div align="center"><span style=font-size:250%;>
  ILCYM predictor: A web platform for predicting pests risk
</div>

Regarding the use of pest risk prediction platforms, end users determine their usefulness through criteria such as the flexibility of the application, with easy-to-interpret results, that such results are convincing and finally that the results have a measurable positive impact. Then it was necessary to develop a web platform that makes geolocated risk predictions based on the ILCYM 4.0 program.

ILCYM software can generate information on the risk of pest establishment and population growth based on temperature data that can support various aspects of pest management. To make ILCYM predictions more accessible to plant health professionals, we propose to create an interactive website (using Shiny) where users can select validated pest/parasitoid models to generate maps or graphs of different population/parasitoid parameters. insect risk for selected locations/geographies. and time frames of interest, including options to mask with crop maps or multiple insect selection.

<b style="font-size:20px;margin-left: 5px">Application components:</b>

<b style="font-size:15px;margin-left: 5px">1. Coordinates (to consult)</b>

These coordinates must be in the format of decimal degrees, which can be presence or consultation records (suspected presence), as defined by the user.

<b style="font-size:15px;margin-left: 5px">2. Phenology</b>

Multiple phenologies have been developed that contain the complete modeling of an insect (pest or natural enemy). These will be part of the list entered in the risk prediction module, and thus generate the prediction in relation to the entered coordinates.

<b style="font-size:15px;margin-left: 5px">3. Climatic Data</b>

The monthly temperature database is considered, where the source may vary, but a good resolution is suggested, such as 2.5 minutes at least (example “Worldclim”), containing in its extension the points or coordinates previously entered by the user. The data format is “FLT”:

• Minimum monthly temperature (January to December)
• Maximum monthly temperature (January to December)

<b style="font-size:15px;margin-left: 5px">4. Crop</b>

In order to optimize the risk analysis, areas can be delimited for a specific crop, this being the host of the insect being evaluated.


