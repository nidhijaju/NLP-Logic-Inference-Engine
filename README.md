# NLP Logic Inference Engine

## Introduction

This project aimed to perform logical reasoning on plaintext English statements. Allowing for automated reasoning, in which the program relies on an internal knowledge bank. It also includes a manual mode, where the program allows you to define your own rules and input statements you would like those rules to be evaluated on.

## Building the application

### Requirements

> dotnet SDK 2.0 or higher
>
> node.js 6.11 or higher
>
> A JS package manager: `yarn` or `npm`

### Automatic

The main section of the application is located in the `.\WorldBuilder` folder. Here you will find 4 batch files. 

### For a release build run: 

> setup.bat
>
> release.bat

### For a development environment run:

> setup.bat
>
> development.bat

then in a separate terminal:

> run.bat

### Purpose of each batch file 

**setup.bat** : Must be run at least once, to install Javascript and Node dependencies. Ensure you have satisfied all the dependencies listed above before running this.

**development.bat** : To start FABLE daemon and launch a development server, use in conjunction with run.bat

**run.bat** : To be run after a development.bat instance has been launched, to view a reactive version of the application.

**release.bat** : To build release version of the application, complete with windows executable.



### Manual

### Building the application

Open a terminal in the `.\WorldBuilder` folder.

> Install JS dependencies: `npm install` or `yarn install`
>
> Install F# dependencies: `dotnet restore`
>
> Start Fable daemon and Webpack: `npm start` or `yarn start`
>
> In a separate terminal, run: `npm run launch` or `yarn run launch`

### Building application for release

Open a terminal in the `.\WorldBuilder` folder.

> Run: `npm run build` or `yarn run build`
>
> Run: `npm run release` or `yarn run release`

You should have now a `.\WorldBuilder\release\NLPLogicEngine-win32-x64` folder. Inside you will find the `NLPLogicEngine.exe`, which will be an executable version of the application.

## Contributors

This project was a term-long group project for High Level Programming module taken in Spring 2020. I collaborated with: Neelesh Ravichandran (Neelesh99), Preet Lalli (preetl), and Agrim Manchanda (agrimmanchanda) all of whom were excellent team mates.
