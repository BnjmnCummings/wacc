// DOM Elements
const waccInput = document.getElementById('waccInput');
const assemblyOutput = document.getElementById('assemblyOutput');
const compileBtn = document.getElementById('compileBtn');
const clearBtn = document.getElementById('clearBtn');
const statusText = document.getElementById('statusText');
const statusBar = document.getElementById('statusBar');
const errorModal = document.getElementById('errorModal');
const errorText = document.getElementById('errorText');
const closeModal = document.querySelector('.close');

// Sample WACC code
const sampleCode = `begin
    int x = 5;
    int y = 10;
    int result = x + y;
    println "The sum is: ";
    println result
end`;

// Set sample code on load
waccInput.value = sampleCode;

// Event Listeners
compileBtn.addEventListener('click', compileCode);
clearBtn.addEventListener('click', clearOutput);
closeModal.addEventListener('click', hideErrorModal);
window.addEventListener('click', (e) => {
    if (e.target === errorModal) {
        hideErrorModal();
    }
});

// Keyboard shortcuts
document.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        e.preventDefault();
        compileCode();
    }
    if (e.key === 'Escape') {
        hideErrorModal();
    }
});

// Functions
async function compileCode() {
    const code = waccInput.value.trim();
    
    if (!code) {
        setStatus('Please enter some WACC code', 'error');
        return;
    }
    
    // Update UI for compilation
    compileBtn.disabled = true;
    compileBtn.textContent = 'Compiling...';
    setStatus('Compiling...', 'compiling');
    assemblyOutput.value = '';
    assemblyOutput.classList.add('loading');
    
    try {
        const response = await fetch('/compile', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ code }),
        });
        
        const result = await response.json();
        
        if (result.success) {
            assemblyOutput.value = result.assembly;
            setStatus('Compilation successful!', 'success');
        } else {
            assemblyOutput.value = '';
            showErrorModal(result.error);
            setStatus('Compilation failed', 'error');
        }
        
    } catch (error) {
        console.error('Error:', error);
        showErrorModal(`Network error: ${error.message}`);
        setStatus('Network error', 'error');
    } finally {
        // Reset UI
        compileBtn.disabled = false;
        compileBtn.textContent = 'Compile';
        assemblyOutput.classList.remove('loading');
    }
}

function clearOutput() {
    assemblyOutput.value = '';
    setStatus('Output cleared', 'success');
}

function setStatus(message, type = '') {
    statusText.textContent = message;
    statusBar.className = `status-bar ${type ? 'status-' + type : ''}`;
    
    // Clear status after 3 seconds for success/error messages
    if (type === 'success' || type === 'error') {
        setTimeout(() => {
            setStatus('Ready');
        }, 3000);
    }
}

function showErrorModal(error) {
    errorText.textContent = error;
    errorModal.style.display = 'block';
}

function hideErrorModal() {
    errorModal.style.display = 'none';
}

// Auto-resize textareas
function autoResize(textarea) {
    textarea.style.height = 'auto';
    textarea.style.height = textarea.scrollHeight + 'px';
}

// Add some helpful examples
const examples = {
    basic: `begin
    int x = 5;
    println x
end`,
    
    array: `begin
    int[] arr = [1, 2, 3, 4, 5];
    println "Array length: ";
    println len arr;
    println "First element: ";
    println arr[0]
end`,
    
    function: `begin
    int add(int x, int y) is
        return x + y
    end
    
    int result = call add(10, 20);
    println "Result: ";
    println result
end`,
    
    conditional: `begin
    int x = 10;
    if x > 5 then
        println "x is greater than 5"
    else
        println "x is not greater than 5"
    fi
end`
};

// Add example buttons (could be added to HTML if desired)
function loadExample(exampleName) {
    if (examples[exampleName]) {
        waccInput.value = examples[exampleName];
        setStatus(`Loaded ${exampleName} example`, 'success');
    }
}

// Initialize
setStatus('Ready');
