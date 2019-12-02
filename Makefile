# Build the readme file
README.md: README.dhall ReadmeExample_norm ReadmeExample.ok
	dhall text --file README.dhall > README.md

# Normalize the readme example to make sure it 
# is correct
ReadmeExample_norm:
	dhall --file ReadmeExample.ok > ReadmeExample_norm

	
