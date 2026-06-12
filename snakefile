rule all: 
    input:
        "params/checkerboard_params.tex",
        "params/local-info-illustration.tex",
        "fig/checkerboard-smoothed-and-trace.png",
        "fig/checkerboard.png",
        "main.tex"
    output: 
        "main.pdf"
    shell: 
        "latexmk -pdf main.tex"

rule checkerboard_local_info: 
    input: 
        "throughput/checkerboard/shapefile",
        "throughput/checkerboard/demographics.csv"
    output: 
        "fig/checkerboard-smoothed-and-trace.png",
        "params/local-info-illustration.tex"
    shell: 
        "Rscript scripts/checkerboard-local-info-viz.R"

rule checkerboard_viz: 
    input: 
        "throughput/checkerboard/shapefile", 
        "throughput/checkerboard/demographics.csv"
    output: 
        "fig/checkerboard.png"
    shell: 
        "Rscript scripts/checkerboard-basic-viz.R"


rule checkerboard_data:        
    output:
        directory("throughput/checkerboard/shapefile"),
        "throughput/checkerboard/demographics.csv", 
        "params/checkerboard_params.tex"
    shell:
        "Rscript scripts/checkerboard-data.R"

rule clean: 
    shell: 
        """
        rm -rf throughput fig main.pdf main.log main.aux main.out main.fls main.fdb_latexmk main.bbl main.bcf main.blg main.run.xml main.xdv params
        """