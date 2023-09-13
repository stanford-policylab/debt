#!/usr/bin/env python3
import os
import urllib.request
import argparse

BASE_URL = "https://debtors-prisons.s3.amazonaws.com"

AVAILABLE_DATA = [
    "mo-adair_county",
    "tx-anderson_county",
    "tx-angelina_county",
    "wi-ashland_county",
    "tx-austin_county",
    "wi-bayfield_county",
    "tx-bell_county",
    "tx-blanco_county",
    "ky-boone_county",
    "tx-bosque_county",
    "tx-bowie_county",
    "ky-boyle_county",
    "tx-brazoria_county",
    "tx-brazos_county",
    "wi-brown_county",
    "ok-cases",
    "tx-chambers_county",
    "tx-cherokee_county",
    "wi-clark_county",
    "mo-clay_county",
    "tx-collin_county",
    "wi-columbia_county",
    "tx-comanche_county",
    "tx-crane_county",
    "tx-denton_county",
    "wi-door_county",
    "wi-douglas_county",
    "tx-eastland_county",
    "tx-el_paso_county",
    "wi-florence_county",
    "wi-fond_du_lac_county",
    "tx-fort_bend_county",
    "tx-galveston_county",
    "tx-gillespie_county",
    "ky-grant_county",
    "tx-gray_county",
    "ky-grayson_county",
    "tx-grayson_county",
    "tx-gregg_county",
    "tx-hardin_county",
    "tx-harris_county",
    "tx-hays_county",
    "tx-henderson_county",
    "tx-hunt_county",
    "tx-jack_county",
    "tx-jasper_county",
    "tx-johnson_county",
    "wi-kenosha_county",
    "tx-kerr_county",
    "wi-kewaunee_county",
    "wi-la_crosse_county",
    "tx-lamar_county",
    "wi-langlade_county",
    "tx-liberty_county",
    "tx-llano_county",
    "tx-madison_county",
    "wi-marathon_county",
    "wi-marinette_county",
    "wi-marquette_county",
    "tx-mclennan_county",
    "wi-milwaukee_county",
    "wi-monroe_county",
    "tx-montgomery_county",
    "tx-moore_county",
    "tx-morris_county",
    "tx-navarro_county",
    "tx-newton_county",
    "tx-nueces_county",
    "tx-ochiltree_county",
    "tx-orange_county",
    "wi-ozaukee_county",
    "tx-palo_pinto_county",
    "tx-pecos_county",
    "tx-reagan_county",
    "tx-refugio_county",
    "tx-rusk_county",
    "wi-rusk_county",
    "wi-sawyer_county",
    "tx-scurry_county",
    "wi-shawano_county",
    "wi-sheboygan_county",
    "tx-stephens_county",
    "la-st_john_the_baptist_parish",
    "tx-taylor_county",
    "wi-taylor_county",
    "tx-titus_county",
    "tx-tom_green_county",
    "tx-travis_county",
    "tx-tyler_county",
    "tx-upshur_county",
    "tx-victoria_county",
    "wi-vilas_county",
    "tx-walker_county",
    "wi-walworth_county",
    "ky-warren_county",
    "wi-waukesha_county",
    "tx-webb_county",
    "tx-wheeler_county",
    "tx-williamson_county",
    "tx-winkler_county",
    "wi-winnebago_county",
]

def progress_bar(name):
    def _(count, block_size, total_size):
        completed = count * block_size
        percent_complete = (completed / total_size) * 100
        print(f"\rDownloading {name}: [{percent_complete:5.1f}%]", end='\r')
    return _


def clean_string(s):
    s = s.lower()
    s = s.replace(' ', '_')
    punctuation = ",./<>?;':\"[]{}\\|`~!@#$%^&*()-_=+"
    s = ''.join(char for char in s if char not in punctuation)
    return s

def normalize_name(state, subgeo):
    state = clean_string(state)
    subgeo = clean_string(subgeo)
    return f"{state}-{subgeo}"

def download_one(state, subgeo, type="rds", download_dir=os.getcwd()):
    name = normalize_name(state, subgeo)
    state = name.split("-")[0]
    if state == "ok" and type == "rds":
        print("Oklahoma data is only available in CSV format.")
        quit()
    if name not in AVAILABLE_DATA or type not in ["rds", "csv"]:
        print(f"Data for '{subgeo}' in '{state}' of type '{type}'"
              "is not available.")
        quit()
    print(f"Downloading data to {download_dir}...")
    url = f"{BASE_URL}/{name}.{type}"
    fp = os.path.join(download_dir, f"{name}.{type}")
    urllib.request.urlretrieve(url, fp, reporthook=progress_bar(name))

def download_all(download_dir=os.getcwd(), type="rds"):
    print(f"Downloading data to {download_dir}...")
    for name in AVAILABLE_DATA:
        if name != "ok-cases":
            url = f"{BASE_URL}/{name}.{type}"
            fp = os.path.join(download_dir, f"{name}.{type}")
        else:
            url = f"{BASE_URL}/{name}.csv"
            fp = os.path.join(download_dir, f"{name}.csv")
        urllib.request.urlretrieve(url, fp, reporthook=progress_bar(name))
        print(80 * ' ', end='\r')
        print(f"Downloaded {name}.{type}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Bulk download data.")
    parser.add_argument("--state", type=str, default=None,
                        help="State to download data for. (Two letter abbrv.)")
    parser.add_argument("--subgeo", type=str, default=None,
                        help="Subgeography to download data for.")
    parser.add_argument("--type", type=str, default="rds",
                        help="Type of data to download. (\"rds\" or \"csv\")")
    parser.add_argument("--download-dir", type=str, default=os.getcwd(),
                        help="Directory to download data to.")
    parser.add_argument("--all", action="store_true",
                        help="Download all available data.")
    parser.add_argument("--available", action="store_true",
                        help="List available data.")

    args = parser.parse_args()

    # If --available was provided, list available data and quit.
    if args.available:
        print("Available data:")
        for name in AVAILABLE_DATA:
            print(f"\t{name}")
        quit()

    # If no args were provided, show help and quit.
    if (args.state is None or args.subgeo is None) and not args.all:
        parser.print_help()
        quit()

    # Ensure download directory exists.
    if not os.path.exists(args.download_dir):
        try:
            os.makedirs(args.download_dir)
        except OSError:
            print(f"Error creating directory {args.download_dir}")
            quit()

    if args.all:
        download_all(download_dir=args.download_dir, type=args.type)
    else:
        download_one(args.state, args.subgeo,
                     download_dir=args.download_dir,
                     type=args.type)
