const express = require('express');
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const cors = require('cors');

const app = express();
const PORT = 3000;

/* Middleware */
app.use(cors());
app.use(express.json());
app.use(express.static('public'));

/* Serve the main HTML file */
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

/* Compile WACC code endpoint */
app.post('/compile', async (req, res) => {
    const { code } = req.body;
    
    if (!code) {
        return res.status(400).json({ error: 'No code provided' });
    }

    try {
        /* Create a temporary file with the WACC code */
        const tempDir = path.join(__dirname, 'temp');
        if (!fs.existsSync(tempDir)) {
            fs.mkdirSync(tempDir);
        }
        
        const tempFileName = `temp_${Date.now()}.wacc`;
        const tempFilePath = path.join(tempDir, tempFileName);
        
        /* Write the code to the temporary file */
        fs.writeFileSync(tempFilePath, code);
        
        /* Get the path to the main WACC directory */
        const waccDir = path.resolve(__dirname, '..');
        
        /* Compile the WACC file using scala */
        try {
            /* Change to WACC directory and run the compiler */
            const command = `cd "${waccDir}" && scala run . -- "${tempFilePath}"`;
            execSync(command, { 
                stdio: 'pipe',
                encoding: 'utf8',
                timeout: 30000 // 30 second timeout
            });
            
            /* The assembly file should be created in the temp directory */
            const assemblyFileName = tempFileName.replace('.wacc', '.s');
            const assemblyFilePath = path.join(tempDir, assemblyFileName);
            
            //* Check if assembly file was created in the temp directory or current directory */
            let assemblyContent = '';
            if (fs.existsSync(assemblyFilePath)) {
                assemblyContent = fs.readFileSync(assemblyFilePath, 'utf8');
            } else {
                /* Check in the main wacc directory */
                const mainAssemblyPath = path.join(waccDir, assemblyFileName);
                if (fs.existsSync(mainAssemblyPath)) {
                    assemblyContent = fs.readFileSync(mainAssemblyPath, 'utf8');
                    /* Clean up the file from main directory */
                    fs.unlinkSync(mainAssemblyPath);
                } else {
                    throw new Error('Assembly file not found after compilation');
                }
            }
            
            /* Clean up temporary files */
            if (fs.existsSync(tempFilePath)) {
                fs.unlinkSync(tempFilePath);
            }
            if (fs.existsSync(assemblyFilePath)) {
                fs.unlinkSync(assemblyFilePath);
            }
            
            res.json({ 
                success: true, 
                assembly: assemblyContent 
            });
            
        } catch (compileError) {
            /* Clean up temporary file */
            if (fs.existsSync(tempFilePath)) {
                fs.unlinkSync(tempFilePath);
            }
            
            /* Parse the error output */
            const errorOutput = compileError.stderr || compileError.stdout || compileError.message;
            res.json({ 
                success: false, 
                error: errorOutput.toString() 
            });
        }
        
    } catch (error) {
        console.error('Compilation error:', error);
        res.status(500).json({ 
            success: false, 
            error: error.message || 'Unknown compilation error' 
        });
    }
});

app.listen(PORT, () => {
    console.log(`WACC Compiler Web Frontend running at http://localhost:${PORT}`);
    console.log('Make sure your WACC compiler is properly set up in the parent directory');
});
