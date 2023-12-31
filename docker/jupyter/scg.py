import collections
import scg_pb2 as scg_pb
import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import itertools as it


def read_scg_file(file_name):
    scg_file = None
    if os.path.isfile(file_name) and file_name.endswith(".semanticgraphdb"):
        with open(file_name, "rb") as f:
            scg_file = scg_pb.SemanticGraphFile()
            scg_file.ParseFromString(f.read())
    return scg_file


def read_scg(workspace):
    scg_folder = os.path.join(workspace, ".semanticgraphs")
    scg = []
    for root, dir, files in os.walk(scg_folder):
        for f in files:
            scg_file = read_scg_file(os.path.join(root, f))
            if scg_file is not None:
                scg.append(scg_file)
    return scg


def create_call_graph(scgs):
    G = nx.DiGraph()
    count = 0
    for scg_file in scgs:
        for node in scg_file.nodes:
            if (
                node.kind
                in {
                    "METHOD",
                    "CONSTRUCTOR",
                    "VALUE",
                    "VARIABLE",
                }
                and node.location.uri != ""
            ):
                G.add_node(node.id, scg_node=node)
                for edge in node.edges:
                    if edge.type == "CALL":
                        count += 1
                        G.add_edge(
                            node.id,
                            edge.to,
                            type="CALL",
                        )

    for n in list(G.nodes()):
        scg_node = G.nodes[n].get("scg_node")
        # filter out not visible in the project nodes
        if G.degree(n) <= 0 or scg_node is None or scg_node.location.uri == "":
            G.remove_node(n)
    print(len(G.nodes()))
    print(len(G.edges()))
    print(count)
    return G


def create_graph(scgs, remove_local=False):
    G = nx.DiGraph()
    for scg_file in scgs:
        for node in scg_file.nodes:
            G.add_node(node.id, scg_node=node, kind=node.kind, display_name = node.displayName)
            for edge in node.edges:
                G.add_edge(node.id, edge.to, type=edge.type)

    for n in list(G.nodes()):
        scg_node = G.nodes[n].get("scg_node")
        # filter out not visible in the project nodes
        if not (
            G.degree(n) > 0
            and scg_node is not None
            and scg_node.location.uri != ""
            and scg_node.kind != "FILE"
            and scg_node.kind != "PACKAGE_OBJECT"
        ):
            G.remove_node(n)

    if remove_local:
        local = [
            n
            for n in G.nodes()
            if G.nodes[n].get("scg_node").properties["isLocal"] == "true" or n.startswith("local")
        ]
        for n in local:
            G.remove_node(n)
    return G


def show_graph(G):
    # Compute the layout using the ForceAtlas2 algorithm
    pos = nx.layout.spring_layout(G)

    # Draw the graph using Matplotlib
    fig, ax = plt.subplots(figsize=(16, 9))
    nx.draw_networkx_edges(G, pos, node_size=5, alpha=0.1, ax=ax)
    nx.draw_networkx_nodes(G, pos, node_size=5, alpha=0.5, ax=ax)
    plt.show()

def show_graph_distribution(G):
    plt.clf()
    #degree_sequence = sorted((d for n, d in G.degree()), reverse=True)[0:100]

    in_degree = list(d for n, d in G.in_degree())
    out_degree = list(d for n, d in G.out_degree())

    scg_in_degrees, scg_in_counts = zip(*sorted(collections.Counter(in_degree).items()))
    scg_out_degrees, scg_out_counts = zip(*sorted(collections.Counter(out_degree).items()))

    plt.scatter(scg_in_degrees, scg_in_counts, marker='o', color='b', label=f'In-Degree SCG', s=20)
    plt.scatter(scg_out_degrees, scg_out_counts, marker='x', color='r', label=f'Out-Degree SCG', s=20)

    avg = sum(in_degree) / len(out_degree)
    plt.axvline(x=avg, color='green', linestyle='--', label='Average Degree')

    plt.xlabel('Degree', fontsize=8)  # Changed 'set_xlabel' to 'xlabel'
    plt.ylabel('Count', fontsize=8)  # Changed 'set_ylabel' to 'ylabel'
    plt.title(f'SCG degree distribution')  # Removed 'graphs[network]'
    plt.grid(True)
    plt.yscale('log')  # Changed 'set_yscale' to 'yscale'
    plt.xscale('log')  # Changed 'set_xscale' to 'xscale'
    plt.legend()

    plt.tight_layout()
    plt.show()

def show_overview(scg_files):
    import matplotlib.pyplot as plt

    fig, axes = plt.subplots(1, 2, figsize=(10, 10), facecolor='#FFFFFF')
    plt.subplots_adjust(wspace=0.25, hspace=-0.3, bottom=0, left=0.05, right=0.95, top=1.0)
    nodes_df = create_nodes_df(scg_files)
    edges_df = create_edges_df(scg_files)
    nodes_kinds = nodes_df.groupby(['kind']).size().sort_values(ascending=False)
    edges_types = edges_df.groupby(['type']).size().sort_values(ascending=False)
    draw_pie(axes[0], "Nodes kinds", nodes_kinds.index, nodes_kinds.values)
    draw_pie(axes[1], "Edges types", edges_types.index, edges_types.values)
    plt.show()

def draw_pie(ax, title, labels, values):
    font_color = '#525252'
    colors = ['#4874f1',
                   '#16cfa8',
                   '#7dd86d',
                   '#f0cd17',
                   '#f06917',
                   '#c12dfc',
                   '#e7165b',
                   '#b67ce2']

    # Filter out entries with values of 0 and their corresponding colors
    non_zero_labels = []
    non_zero_values = []
    non_zero_colors = []
    for label, value, color in zip(labels, values, colors):
        if value/sum(values) * 100 > 0.7:
            non_zero_labels.append(label)
            non_zero_values.append(value)
            non_zero_colors.append(color)

    ax.pie(
        non_zero_values,
        labels=non_zero_labels,
        colors= non_zero_colors,
        startangle=30,
        textprops={'color': font_color, 'fontsize': 10},
        #wedgeprops=dict(width=.5),  # For donuts
        autopct=lambda p: '{:.1f}%'.format(p)
    )
    ax.set_title(title, y=1)


def create_nodes_df(scg_files):
    nodes = [n for file in scg_files for n in file.nodes]

    def extractDict(n):
        attributes = {}
        attributes["id"] = n.id
        attributes["displayName"] = n.displayName
        attributes["kind"] = n.kind
        attributes["package"] = n.properties["package"]
        attributes["file"] = n.location.uri.split("/")[-1]
        attributes["isLocal"] = n.properties["isLocal"] == "true"
        attributes["startLine"] = n.location.startLine
        attributes["startCharacter"] = n.location.startCharacter
        attributes["endLine"] = n.location.endLine
        attributes["endCharacter"] = n.location.endCharacter
        return attributes

    dict = [extractDict(n) for n in nodes if n]
    return pd.DataFrame(dict)

def create_edges_df(scg_files):
    nodes = [n for file in scg_files for n in file.nodes]
    edges = [(e, n.id) for n in nodes for e in n.edges]

    def extractDict(edge, id):
        attributes = {}
        attributes["from"] = id
        attributes["to"] = edge.to
        attributes["type"] = edge.type
        attributes["startLine"] = edge.location.startLine
        attributes["startCharacter"] = edge.location.startCharacter
        attributes["endLine"] = edge.location.endLine
        attributes["endCharacter"] = edge.location.endCharacter
        return attributes

    dict = [extractDict(e, id) for (e, id) in edges if e]
    return pd.DataFrame(dict)

def find_method_similarities(G, s_min = 5, s_min_p = 50):

    methods = [
        n
        for n in G.nodes()
        if G.nodes[n].get("scg_node").kind == "METHOD"
    ]
    methods_comb = list(it.combinations(methods, 2))

    def owner(G, n):
        return [
            p
            for p in G.predecessors(n)
            if G[p][n]["type"] == "DECLARATION"
        ]


    similar = []
    for (
        n1,
        n2,
    ) in methods_comb:
        n1_n = set(G.successors(n1))
        n2_n = set(G.successors(n2))
        s = len(set(n1_n).intersection(n2_n))
        n1_p = 0 if s == 0 else int(s / len(n1_n) *100)
        n2_p = 0 if s == 0 else int(s / len(n2_n) *100)
        if s >= s_min and (
            n1_p >= s_min_p and n2_p >= s_min_p
        ):
            if owner(G, n1) == owner(G, n2):
                similar.append(
                    (
                        n1,
                        n2,
                        s,
                        n1_p,
                        n2_p,
                    )
                )

    df = pd.DataFrame.from_records(
        similar,
        columns=[
            "n1",
            "n2",
            "#s",
            "%n1",
            "%n2",
        ],
    )
    return df


def main():
    pass


if __name__ == "__main__":
    main()
