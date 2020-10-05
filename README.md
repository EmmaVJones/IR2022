# IR2022
This repository holds scripts to organize and assess VDEQ data for 2022 Integrated Report. 

## Overview
This project outlines a streamlined assessment methodology acoss all DEQ regions for lacustrine and riverine assessments. Prior to any assessment decisions, all DEQ stations that fall within the assessment window (2015-2020) must be properly attributed with metadata (assessment unit and water quality standards information). After these links have been made by regional assessment staff, a data analyst can combine this information with stations to run the assessment methods across all data in the window. After automated assessment methods are run, regional assessment staff may dig into data and decisions with interactive shiny applications that utilize data visualization to further explain assessment calls. After assessors review automated assessment decisions, the automated assessment output (stations table) may be uploaded to CEDS WQA to finish assessment procedures in the ATTAINS-like database schema. 

The following components are necessary to complete the automated assessment and should be completed in order. 

### 0.CitmonDataOrganization
The citizen/non agency monitoring data needs to be organized into a single, "conventionals-like" format from the original, disparate data structures received from various monitoring groups. This project helps the Citizen Monitoring Coordinator organize stations first by spatial verification among data received, compare to previous DEQ-processed citizen/non agency stations, and transform all parameters collected into an initial format for further organization.

### 1.preprocessData
This workflow/application institutes a new method for DEQ to store WQS attribution and update links as WQS change. The shiny application presents regional assessors with WQS suggestions based on a series of spatial joins that are completed by a data analyst prior to stations appearing in the app. Assessors choose the most appropriate WQS to permanently link to a DEQ station. If WQS layer change upstream of these links, the links will be broken and the data analyst can quickly identify stations affected by WQS updates, rerun spatial joins based on new WQS, and present new WQS information to assessors in the application for review. If underlying WQS is not changed for a particular station, then no additional work is required. This process is only completed every two years in preparation for the IR. 

These WQS links provide agency staff access to manually QAed WQS information for all agency business needs.

Like the WQS attribution, assessment units are spatially joined to new DEQ stations in preparation for each IR as a starting point for the assessment process. An AU review module exists inside the Assessment Metadata Review tool to complete these activities. 

The application is hosted on DEQ's internal R Connect service and assessors save data directly to the centralized server. Reviewed WQS information helps feed the WQM Stations (full) dataset hosted on the DEQ GIS Staff Application for all agency employees to reference.

### 2.organizeMetadata
After all WQS and AU information exists for stations in a given IR window, this information must be joined to data collected in the window to enable automated assessment methods. This project verifies all stations that need AU and WQS information (stations sampled in an assessment window) are properly attributed and then completes joins to make further analyses possible.

### 3.automatedAssessment
After stations are appropriately attributed, these scripts analyze all stations based on the DEQ Assessment Guidance. New for 2022IR, assessors will have access to these scripts in the form of a shiny application hosted on the Connect server to allow easier rerunning of assessments based on decisions made during the assessment process (e.g. splitting assessment units, data updates, bioassessment decisions, etc.).

### 4.LakesApplication
This project builds the interactive shiny application for lakes assessment, taking the static stations table output and transforming it into a data visualization experience for assessors to better understand automated assessment decisions.

### 4.RiverineApplication
This project builds the interactive shiny application for riverine assessment, taking the static stations table output and transforming it into a data visualization experience for assessors to better understand automated assessment decisions.