Multibeam sonars are advanced scientific tools for estimating fish school volume and density, 
using multiple beams to provide comprehensive size measurements of detected targets. 
However, challenges remain in accurately estimating target dimensions due to beam geometric expansion and overlap, 
particularly in athwart-beam measurements, which tend to be overestimated with increasing distance from the transducer. 

This repository contains the R scripts for an interactive shinny app that simulates distortion 
caused by beam overlap and expansion in multibeam sonars using simple geometric equations. Users can define sonar characteristics, 
such as the number of beams, swath opening, and degree of overlap, and specify an elliptical target's dimensions, orientation, 
and distance from the transducer. The application estimates and visualizes the true and distorted shapes of the target, 
calculating the level of distortion. It can run simulations in both forward and inverse directions, 
either simulating the distortion of a true school or correcting the shape of a distorted school. 
This tool aims to enhance the interpretation of multibeam sonar signals and improve the accuracy of target dimension estimates, 
facilitating more effective use of these sonars in scientific research.

Information about the equations and code used to build the script can be found in the follwing paper and preprint:

https://www.researchgate.net/publication/366673185_Correction_of_beam_overlap-induced_athwart_distortion_in_multibeam_sonars

https://www.researchgate.net/publication/381045255_A_web-based_interactive_application_to_simulate_and_correct_distortion_in_multibeam_sonars

The scripts to simulate a Simrad SN90 were created by Guillermo Boyra in February 2023. 
The app was generalized for different types of multibeam sonar systems in May 2024. 
