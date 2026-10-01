ALL_CITIES = ["Detroit",  "Atlanta",  "Milwaukee", "Washington_DC"]

rule all: 
    input:
        "main.pdf"

rule paper: 
    input:
        "params/checkerboard-params.tex",
        "params/local-info-illustration.tex",
        "fig/checkerboard-smoothed-and-trace.png",
        "fig/checkerboard.png",
        "fig/simplex-entropy.png", 
        "fig/simplex-cumulative.png", 
        "fig/simplex-jensen-info.png",
        "fig/smoother-curves.png",
        "fig/aggregation-viz.png",
        "fig/local-info.png",
        "main.tex",
        "macros.tex",
        "content.tex",
        "refs.bib",
    output: 
        "main.pdf"
    shell: 
        "latexmk -pdf main.tex"

# SPATIAL KERNEL SMOOTHING FIGURE
EXAMPLE_CITIES = ["Atlanta", "Detroit", "Washington_DC"]
rule smoother_fig: 
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/smoother-curves.png"
    shell: 
        "Rscript scripts/smoother-curves.R {EXAMPLE_CITIES}"

rule dot_fig:
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/dot-viz.png"
    shell: 
        "Rscript scripts/dot-viz.R {EXAMPLE_CITIES}"
        "magick fig/dot-viz.png -trim fig/dot-viz.png"

rule aggregation_fig: 
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/aggregation-viz.png"
    shell: 
        "Rscript scripts/hclust-viz.R {EXAMPLE_CITIES}"

LOCAL_INFO_CITY = "Milwaukee"
rule local_info_fig: 
    input: 
        "throughput/local-info/{city}.rds".format(city=LOCAL_INFO_CITY)
    output: 
        "fig/local-info.png"
    shell: 
        "Rscript scripts/city-local-info-viz.R {LOCAL_INFO_CITY}"





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


# various simplex diagrams
rule simplex: 
    input: 
    output: 
        "fig/{fig}.png"
    shell: 
        "Rscript scripts/{wildcards.fig}.R"
        "magick fig/simplex-entropy-cumulative.png -trim fig/simplex-entropy-cumulative.png"
        "magick fig/simplex-isocontours.png -trim fig/simplex-isocontours.png"
        "magick fig/simplex-jensen-info.png -trim fig/simplex-jensen-info.png"

rule grid_diagram: 
    input: 
    output: 
        "fig/dot-diagram.png"
    shell: 
        "Rscript scripts/grid-diagrams.R"
        "magick fig/dot-diagram.png -trim fig/dot-diagram.png"

rule checkerboard_viz: 
    input: 
        "throughput/checkerboard/shapefile", 
        "throughput/checkerboard/demographics.csv"
    output: 
        "fig/checkerboard.png"
    shell: 
        "Rscript scripts/checkerboard-viz.R"

rule checkerboard_data:        
    output:
        directory("throughput/checkerboard/shapefile"),
        "throughput/checkerboard/demographics.csv", 
        "params/checkerboard-params.tex"
    shell:
        "Rscript scripts/checkerboard-data.R"

rule clean: 
    shell: 
        """
        rm -rf throughput fig params && rm *.pdf *.log *.aux *.out *.fls *.fdb_latexmk *.bbl *.bcf *.blg *.run.xml *.xdv *.tdo *.synctex.gz *.dvi
        """