# final project

<!-- badges: start -->
<!-- badges: end -->

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-package">About The Package</a>
      <ul>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li>
      <a href="#data-used">Data Used</a>
    <li><a href="#examples ">Examples </a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ol>
</details>

<!-- ABOUT THE PACKAGE -->   

## About the package   

The goal of pkgrpEArmy is to generate a shiny app by calling the function "militaryapp()".   
Please see more the vignette section "my_army_app" and "description" for more info.

## Installation

You can install the development version of pkgrpEArmy from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ManoelPidoux/PTDS-Final-Project-Group-E")
```

<!-- DATA USED -->
## Data Used

The following websites were used to export data: 
1. [Points Score Data](https://www.baspo.admin.ch/content/baspo-internet/fr/sportfoerderung/breitensport/fitnesstest-armee-fta-rekrutierung/_jcr_content/contentPar/accordion/accordionItems/268_1649923349772/accordionPar/downloadlist_copy_92/downloadItems/74_1632920294073.download/f_Poster_FTA_Rekrutierung_2021_Tabelle_screen.pdf)
2. [Sport Performances](https://www.miljobs.ch/fr/funtkionen-von-a-bis-z)

<!-- EXAMPLES -->   

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pkgrpEArmy)
## militaryapp(1)
```
The function when called should be without any arguments inside.    

``` r
library(pkgrpEArmy)
## Convert(jumping = -2, ball_toss = 5, equilibrium = 30, planking = 50, running = 300)
```
The function will return an error with negative values inside it.     

``` r
library(pkgrpEArmy)
## Orientation(list("a", "b", "c"))
```
The function will return an error with non-numerical values inside it. 

<!-- LICENSE -->
## License

Distributed under the MIT License. See [LICENSE](LICENSE.md/) for more information.

<!-- CONTACT -->
## Contact
                                   
If you have any question feel free to contact us in writing an issue in this repository. 

<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements
*  The team: Alistair Bisol, Andrea Ferrazzo, Jacopo Beghetto and Manoel Pidoux 
