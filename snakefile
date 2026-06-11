rule all: 
    input:
        "fig/checkerboards.png",
        "main.tex"
    shell: 
        "latexmk -pdf main.tex"

rule basic_checkerboards:        
    output:
        "fig/checkerboards.png"
    shell:
        "Rscript scripts/checkerboards.R"