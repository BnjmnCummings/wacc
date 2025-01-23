import yaml
import subprocess

def main():
    # Path to the YAML file
    yaml_file = 'selectedTests.yml'

    # Read the YAML file
    with open(yaml_file, 'r') as file:
        data = yaml.safe_load(file)

    # Extract the tests
    tests = extract_tests(data)

    # Run each test
    processes = list(map(run_test, tests))
    return_codes = list(map(lambda p: p.returncode, processes))
    # If any test fails (), exit with a failure
    exit(any(return_codes))

# Function to recursively extract test names
def extract_tests(data):
    tests = []
    for key, value in data.items():
        if isinstance(value, list):
            for item in value:
                tests.append(f"{key}_{item}")
        elif isinstance(value, dict):
            nested_tests = extract_tests(value)
            for nested_test in nested_tests:
                tests.append(f"{key}_{nested_test}")
    return tests

def run_test(test_name):
    command = f'scala test . --test-only "{test_name}*"'
    p = subprocess.run(command, shell=True)
    print(p.stdout)
    return p

main()