@echo off

title Building Application
echo Installing dependencies
call npm install

title Building Application
echo Installing dependencies 1
call dotnet restore
