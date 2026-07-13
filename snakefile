CITIES = ["Detroit", "Boston"]

rule all: 
    input:
        "params/checkerboard_params.tex",
        "params/local-info-illustration.tex",
        "fig/checkerboard-smoothed-and-trace.png",
        "fig/checkerboard.png",
        expand("throughput/geo/{city}.rds", city=CITIES),
        expand("throughput/local-info/{city}.rds", city=CITIES),
        "main.tex"
    output: 
        "main.pdf"
    shell: 
        "latexmk -pdf main.tex"

rule city_local_info:
    input:
        "throughput/geo/{city}.rds"
    output:
        "throughput/local-info/{city}.rds"
    params: 
        city="{city}"
    shell:
        "Rscript scripts/city-local-info.R {params.city}"

rule grab_city_data:
    input:
        "assumptions/cities.csv"
    output:
        "throughput/geo/{city}.rds"
    params: 
        city="{city}"
    shell:
        "Rscript scripts/grab-city-data.R {params.city}"


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