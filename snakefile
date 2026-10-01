ALL_CITIES = ["Detroit",  "Atlanta",  "Milwaukee", "Washington_DC"]

rule all: 
    input:
        "main.pdf",
        "article.pdf",
        "supplementary.pdf",

rule arXiv:
    input: 
        "main.tex",
        "macros.tex",
        "content.tex",
        "refs.bib",
        "fig/dot-diagram.png",
        "fig/dot-viz.png",
        "fig/simplex-isocontours.png",
        "fig/simplex-jensen-info.png",
        "fig/smoother-curves.png",
        "fig/aggregation-viz.png",
        "fig/local-info.png",
    output: 
        "arXiv.zip"
    shell:
        """
        mkdir -p arXiv
        cp main.tex arXiv/
        cp macros.tex arXiv/
        cp content.tex arXiv/
        cp refs.bib arXiv/
        cp fig/dot-diagram.png arXiv/
        cp fig/dot-viz.png arXiv/
        cp fig/simplex-isocontours.png arXiv/
        cp fig/simplex-jensen-info.png arXiv/
        cp fig/smoother-curves.png arXiv/
        cp fig/aggregation-viz.png arXiv/
        cp fig/local-info.png arXiv/
        zip -r arXiv.zip arXiv
        rm -rf arXiv
        """
        


rule paper: 
    input:
        "fig/dot-diagram.png",
        "fig/dot-viz.png",
        "fig/simplex-isocontours.png",
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

rule article: 
    input:
        "fig/dot-diagram.png",
        "fig/dot-viz.png",
        "fig/simplex-isocontours.png",
        "fig/simplex-jensen-info.png",
        "fig/smoother-curves.png",
        "fig/aggregation-viz.png",
        "fig/local-info.png",
        "article.tex",
        "macros.tex",
        "content.tex",
        "refs.bib",
    output: 
        "article.pdf", 
        "supplementary.pdf"
    shell: 
        """
        latexmk -pdf article.tex
        latexmk -pdf supplementary.tex
        """

# SPATIAL KERNEL SMOOTHING FIGURE
EXAMPLE_CITIES = ["Atlanta", "Detroit", "Washington_DC"]
rule smoother_fig: 
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/smoother-curves.png"
    shell: 
        """
        Rscript scripts/smoother-curves.R {EXAMPLE_CITIES}
        magick fig/smoother-curves.png -trim fig/smoother-curves.png
        """

rule dot_fig:
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/dot-viz.png"
    shell: 
        """
        Rscript scripts/dot-viz.R {EXAMPLE_CITIES}
        magick fig/dot-viz.png -trim fig/dot-viz.png
        """

rule aggregation_fig: 
    input: 
        expand("throughput/geo/{city}.rds", city=EXAMPLE_CITIES)
    output: 
        "fig/aggregation-viz.png"
    shell: 
        """
        Rscript scripts/hclust-viz.R {EXAMPLE_CITIES}
        magick fig/aggregation-viz.png -trim fig/aggregation-viz.png
        """

LOCAL_INFO_CITY = "Milwaukee"
rule local_info_fig: 
    input: 
        "throughput/local-info/{city}.rds".format(city=LOCAL_INFO_CITY)
    output: 
        "fig/local-info.png"
    shell: 
        """
        Rscript scripts/city-local-info-viz.R {LOCAL_INFO_CITY}
        magick fig/local-info.png -trim fig/local-info.png
        """

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

# various simplex diagrams
rule simplex: 
    input: 
    output: 
        "fig/simplex-isocontours.png"
    shell: 
        """
        Rscript scripts/simplex-isocontours.R
        magick fig/simplex-isocontours.png -trim fig/simplex-isocontours.png
        """

rule simplex_info: 
    input: 
    output: 
        "fig/simplex-jensen-info.png"
    shell: 
        """
        Rscript scripts/simplex-jensen-info.R
        magick fig/simplex-jensen-info.png -trim fig/simplex-jensen-info.png
        """


rule grid_diagram: 
    input: 
    output: 
        "fig/dot-diagram.png"
    shell: 
        """
        Rscript scripts/grid-diagrams.R
        magick fig/dot-diagram.png -trim fig/dot-diagram.png
        """


rule clean: 
    shell: 
        """
        rm -rf throughput fig params && rm *.pdf *.log *.aux *.out *.fls *.fdb_latexmk *.bbl *.bcf *.blg *.run.xml *.xdv *.tdo *.synctex.gz *.dvi
        """