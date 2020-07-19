@echo off

title Building Application
echo Parsing and building application
call npm run build

title Creating Release Executable
echo Optimizing and creating executable
call npm run release