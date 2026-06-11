rule all: 
    input:
        "fig/checkerboard.png",
        "main.tex"
    output: 
        "main.pdf"
    shell: 
        "latexmk -pdf main.tex"


rule checkerboard_viz: 
    input: 
        "throughput/checkerboard/shapefile", 
        "throughput/checkerboard/demographics.csv"
    output: 
        "fig/checkerboard.png"
    shell: 
        "Rscript scripts/viz-checkerboards.R"


rule checkerboard_data:        
    output:
        directory("throughput/checkerboard/shapefile"), 
        "throughput/checkerboard/demographics.csv"
    shell:
        "Rscript scripts/make-checkerboard-data.R"

rule setup: 
    output:
        directory("throughput"), 
        directory("fig")
    shell:
        "Rscript scripts/setup.R"

rule clean: 
    shell: 
        """
        rm -rf throughput/* fig/* main.pdf main.log main.aux main.out main.fls main.fdb_latexmk
        """