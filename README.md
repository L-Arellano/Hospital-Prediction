# Hospital Readmission Predictions Using R
<div id="top"></div>

<!-- PROJECT SHIELDS -->
[![Issues][issues-shield]][issues-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/L-Arellano/Hospital-Prediction">
  </a>

<h3 align="center">Twitter Sentiment Analysis with Python</h3>

  <p align="center">
    Predicting hospital readmissions using R based on patient data and health indicators.
    <br />
    <a href="https://github.com/L-Arellano/Hospital-Prediction"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    ·
    <a href="https://github.com/L-Arellano/Hospital-Prediction/issues">Report Bug</a>
    ·
    <a href="https://github.com/L-Arellano/Hospital-Prediction/issues">Request Feature</a>
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About the project

The code preprocesses hospital readmission data, training and evaluating three prediction models: Logistic Regression, Random Forest, and Decision Tree, to estimate the likelihood of a patient being readmitted. Following modeling, the top 100 predictions from the logistic regression model are extracted, saved to a CSV, and further analyzed to explore patterns such as days in inpatient care, age distribution, and presence of specific health conditions, providing insights into the characteristics of high-risk readmission patients.

### Built With

* [R](https://www.r-project.org/)
* [glm (Generalized Linear Model)](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm)
* [ranger (Random Forests)](https://cran.r-project.org/web/packages/ranger/ranger.pdf)
* [caret (Classification and Regression Training)](https://cran.r-project.org/web/packages/caret/vignettes/caret.html)
* [ggplot2 (Data Visualization)](https://ggplot2.tidyverse.org/)
* [dplyr (Data Manipulation)](https://dplyr.tidyverse.org/)

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

To run the provided R code, you'll need to have these R packages:
*glm
*ranger
*caret
*ggplot2
*dplyr

### Installation

If you haven't installed the above R packages, execute these commands in your R console:

```sh
install.packages("glm")
install.packages("ranger")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
```
<!-- CONTACT -->
## Contact

Luis Arellano - luisaur10@hotmail.com

Project Link: [https://github.com/L-Arellano/Hospital-Prediction](https://github.com/L-Arellano/Hospital-Prediction)

<p align="right">(<a href="#top">back to top</a>)</p>




<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[issues-shield]: https://img.shields.io/github/issues/L-Arellano/Hospital-Prediction.svg?style=for-the-badge
[issues-url]: https://github.com/L-Arellano/Hospital-Prediction/issues
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/luis-arellano-a312631bb/
[product-screenshot]: images/screenshot.png
