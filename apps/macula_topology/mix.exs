defmodule Maculatopology.MixProject do
  use Mix.Project

  def project do
    [
      app: :macula_topology,
      version: "0.2.0",
      language: :erlang,
      description: "Macula Topology - Network topology management",
      package: package(),
      deps: deps()
    ]
  end

  defp deps do
    []
  end

  defp package do
    [
      name: "macula_topology",
      files: ["src", "rebar.config", "rebar.lock", "README.md", "LICENSE"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/macula-io/macula"}
    ]
  end
end
