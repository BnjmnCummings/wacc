package wacc.utilities

import java.io.File

// Function to recursively find all files in the given directory
// Written by chatgpt
def searchDir(dir: File): List[String] = {
    if (dir.exists && dir.isDirectory) {
        // List to collect all file paths
        val filePaths = dir.listFiles.filter(_.isFile).map(_.getAbsolutePath).toList

        // Recursively search in subdirectories
        val subDirFiles = dir.listFiles.filter(_.isDirectory).flatMap(searchDir).toList

        // Combine files from current directory and subdirectories
        filePaths ++ subDirFiles
    } else {
        List()  // Return empty list if the path is not a valid directory
    }
}

