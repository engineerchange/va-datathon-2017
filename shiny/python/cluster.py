#
#FLASK_APP=cluster.py flask run
from flask import Flask
from flask import jsonify
from flask import request
import pandas as pd
import ast

from sklearn import cluster, datasets, mixture
from sklearn.preprocessing import StandardScaler

app = Flask(__name__)

@app.route("/")
def get_assignments():

	key2val = request.args.to_dict()

	n_clusters = int(key2val["n_clusters"]) if "n_clusters" in key2val else 3

	data = pd.read_csv("../../data/clustering/mergedforCluster.csv")
	fips = data["FIPS Code"].copy().values
	columns = set(data.columns.values)
	del data["FIPS Code"]

	X = data.fillna(0)

	if "subset" in key2val and len(ast.literal_eval(key2val["subset"])) > 0:
		subset = [s for s in ast.literal_eval(key2val["subset"]) if s in columns]
		X = X[subset]		

	db = cluster.AgglomerativeClustering(n_clusters=n_clusters, linkage="ward")
	X = StandardScaler().fit_transform(X)
	db.fit(X)

	response = []

	fips = [str(f) for f in fips]
	labels = [int(l) for l in db.labels_]

	for kv in zip(fips, labels):
		response.append({"fips": kv[0], "cluster": kv[1]})

	return jsonify(response)
