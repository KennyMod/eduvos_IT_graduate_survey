# Eduvos IT Graduates Survey

This project is an interactive dashboard built with **R Shiny** that visualizes the technology usage among IT graduates in Eduvos Campuses. It displays simulated survey data across various technology categories, including programming languages, databases, web frameworks, and other tools.

Link to the dashboard can be accessed [here](https://kennymod.shinyapps.io/r_shiny_assignment/).

## Features

-   **Interactive Filtering:**
    -   Filter campus by dropdown.
    -   Demographics showing different data and charts for the IT graduates
    -   Summary filter that gives a summary of the Top data tools and employability rate.
-   **Dynamic Visualizations:**
    -   Bar plots for each technology category generated using `ggplot2`.
    -   Pie & Donuts plot for Demographics using `ggplot2`.
-   **Modern UI:**
    -   Styled with `shinythemes` for a contemporary look.

## Technologies Used

-   **R**
-   **Shiny**
-   **shinythemes**
-   **tidyverse**
-   **viridis**
-   **rsconnect**

## Project Structure

-   **Data Generation:**\
    A CSV dataset containing survey data for IT graduates was provided for the analysis of this IT graduate survey. Each record includes responses for programming languages, databases, web frameworks, other tools, years of experience, employment status and many mre columns but only 13 columns of the total 80+ columns was analysed.

-   **User Interface (UI):**\
    The UI consists of a sidebar for filtering options and a main panel with a tabset for each technology category.

-   **Server Logic:**\
    The server side uses reactive expressions to filter the data based on user input and renders plots for each category. This is provided by the package from shiny.

## Prerequisites

-   **R** (version 4.3)
-   **RStudio** (optional, but recommended)

## Installation

1.  **Clone the Repository**\
    Open your terminal and run:

    ``` bash
    git clone <repository-url>
    cd <repository-folder>
    ```

2.  **Install Required Packages** `install.packages(c("shiny", "shinythemes", "tidyverse", "viridis", "rsconnect"))`

3.  **Running the Application**

    -   Open the project or the main application file (e.g., app.R).
    -   Click on the "Run App" button. Once the application starts, it will launch in your default web browser, and you can interact with the dashboard.
