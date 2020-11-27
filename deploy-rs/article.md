# Our New Nix Deployment Tool: `deploy-rs`

At Serokell, we use a lot of Nix. While it helps us in so many ways, we felt that the options for Nix+NixOS deployments were actually quite lacking, and no existing tool fit the requirements for our infrastructure.

We tried out [morph](https://github.com/DBCDK/morph), [NixOps](https://github.com/NixOS/nixops), some custom per-project deployment scripts, even plain old `nixos-rebuild`, and some non-Nix solutions, but none of them solved every problem.

Therefore, we decided to look into creating our own solution: [`deploy-rs`](https://github.com/serokell/deploy-rs).

We found that it must be:

- **Remote.** We don't (always) want to perform the builds on the target server.
- **Stateless.** Synchronizing state data between each of our developers and automatic deployments is not worth any benefit.
- **`sudo`-compatible.** We don't want to need a root login for deployments, even if it is a full NixOS system closure.
- **Safe.** We don't want to waste Nix's amazing ability to roll back if activation fails.
- **Re-usable.** The program must fit every one of our Nix deployments.

There are a lot of NixOS deployment tools that already fit these needs, though we would still have to give root access to our CI/CD machines, or use a non-Nix solution for all app deployments, both of which are undesirable.

The solution we came up with is to _not_ make or use a NixOS deployment tool at all, but instead create a _Nix_ deployment tool that supports NixOS and any other type of Nix profile, system or otherwise.

The result was [this](https://github.com/serokell/deploy/blob/master/deploy.sh), a wonderfully simple bash script. It could not only deploy a NixOS system, but also any profile you want (as outlined in [this](https://github.com/serokell/deploy/blob/master/examples/simple/flake.nix) example).

It makes great use of Nix's new [flakes](https://nixos.wiki/wiki/Flakes) feature, and treats NixOS deployments the same way it treats a deployment of any other profile, giving us the ability to deploy any application's root-less profiles alongside the NixOS profile.

This was an amazing first prototype and showed that Nix deployment could still be improved upon, though it still had a number of issues. It was in Bash (which is fine, though risky to continue to develop given it is critical infrastructure), had theoretical rollback issues if certain properties changed, and had some since-abandoned ideas in it (such as `bootstrap`) from previous experimentation.

`deploy-rs` is a full re-implementation of the ideas behind this initial tool. We had already struck a good idea, but just needed to polish it.

This tool follows the same pattern but expands upon it, and is written entirely in Rust so we can enjoy additional safety, expressiveness, and speed. We kept the same design, the flakes, the multi-profile implementation, but improved the interface and added more functionality.

## Features

We've implemented a lot of the basic features present in a lot of the alternative NixOS deployment tools, though our own tool has some distinct features that solve issues in both NixOS and non-NixOS deployment profiles.

For a more complete list, you should read the official [README](https://github.com/serokell/deploy-rs/blob/master/README.md) of the project.

### Multi-profile

A standard NixOS deployment tool will let you deploy a NixOS configuration to a machine. However `deploy-rs` is a _Nix_ deployment tool with no explicit dependency on NixOS at all.

For each of the servers (called nodes), you may have any number of profiles, which get deployed independently of each other, you can have one be a NixOS profile installing as root, another be some other type of profile deploying as any user you want.

```nix
{
  deploy.nodes.example = {
    hostname = "localhost";
    profiles = {
      system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.example;
      };
      hello = {
        user = "hello";
        path = deploy-rs.lib.x86_64-linux.activate.custom self.defaultPackage.x86_64-linux "./bin/activate";
      };
    };
  };
}
```

### Magic rollback

As mentioned before, one of the things that make Nix/NixOS great is the ability to roll back. This is something many of the other tools support, but only when activation fails. We decided to take this to the next level. `deploy-rs` (if `magic-rollback` is enabled) will create a "canary" file after profile activation, which will get deleted by the deploying end, otherwise the profile will roll-back and re-activate after 30 seconds (though the timeout is configurable).

This means we don't have to worry about causing disasters as we modernize our infrastructure to use our new tooling. If we remove our own keys, mess up the networking, or otherwise prevent ourselves from accessing the server, the mistake will only last for 30 seconds at most.

### Static activation paths

If you have read through the code of `deploy` above, you might have noticed this comment:

```bash
# Assuming that activation command didn't change
eval "$SUDO $activate"
```

This is actually important, as it is making the assumption that the activation command to re-activate the previous generation is the same as the one used to activate the current generation. This means that if you change your activation command and it results in a failure, the rollback will fail too.

In `deploy-rs` we solved this by having the activation path be static and standardized: `deploy-rs` will always run the file in `${yourProfile}/deploy-rs-activate`. To bring back some of the convenience of the `activate` field in our bash ancestor, we include a utility called `lib.activate` in our flake. To activate with a custom command, you can use `deploy-rs.lib.x86_64-linux.activate.custom pkgs.hello "./bin/hello"`, to activate a NixOS system you can use `deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.some-random-system`, and if you just want to write the profile without activating anything, you can use `deploy-rs.lib.x86_64-linux.activate.noop`.

### Interactive mode

If you provide the `-i` flag, `deploy-rs` will print everything that will be activated in toml and prompt you before continuing.

```
[examplecomputer.system]
user = "root"
ssh_user = "exampleuser"
path = "/nix/store/xxrgxdg8x9zy8p0ky3d2hff4wf5igjpk-activatable-nixos-system-examplecomputer-21.03.20201119.a322b32"
hostname = "localhost"
ssh_opts = []

[examplecomputer.exampleuser-home-manager]
user = "exampleuser"
ssh_user = "exampleuser"
path = "/nix/store/k32k88vbyx9yqk7fk80bk0vcybzbkhra-activatable-home-manager-generation"
hostname = "localhost"
ssh_opts = []

 INFO  deploy > Are you sure you want to deploy these profiles?
> 
```

### Non-flake support

While the implementation and mere concept of this tool stem from the new unstable-only Nix flakes command, we've made sure that this tool will work with any stable version of Nix too.

Once you have created your flake and flake.lock with a newer version of Nix, you can use `flake-compat` to make your flake accessible on older Nix versions through a `default.nix`. `deploy-rs` will automatically detect the lack of flakes support, read from the `default.nix`, and perform certain workarounds (such as manually building the `.checks` attribute in absence of `nix flakes check`).

### Automatic checks

`deploy-rs`'s flake ships with some utility checks to put in your flake checks, and will automatically check all of them before deployment (unless `--skip-checks` is specified). This helps you make sure there are no issues in your infrastructure before each deployment.

You can use our included checks by simply adding this to your `flake.nix`:

```nix
{
  checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
}
```

### Passing Nix arguments

Any trailing argument given to `deploy-rs` will be passed on to Nix itself, so you can do things like `deploy . -- --override-input nixpkgs ./nixpkgs`.

## How we use it

There are examples already included in the GitHub repository, though if you want to see some real-world usage, check out our [web app cluster definition](https://github.com/serokell/pegasus-infra), where we exclusively utilize `deploy-rs` for NixOS deployments, and [one of the apps we run on it](https://github.com/serokell/hackage-search) which utilizes `deploy-rs` for deploying the service without root.

It's worth noting that although in these examples we have the systemd service in the NixOS part of the deployment, and just a simple `systemctl restart` on the app end, you can just as easily use systemd user services to accomplish the same thing without sudo rules. We just chose not to.

You can even use it to deploy a [home-manager](https://github.com/rycee/home-manager) configuration without root.

## The future

It has reached a feature-complete stage and passed review by our own team, though our work is far from done. We want to grow `deploy-rs` into the ultimate deployment tool, with well-designed tooling for all of our use cases.

Our next immediate goal (alongside bugfixes and code cleanup) is to support systemd portable services, to avoid the issue in the above section where we are using system-level services for user applications.

We're also considering the idea of a module system to add features such as a module for automatic `kexec` lustration of servers allowing you to convert systems to NixOS during deployment, a module for integrating with [Vault](https://www.vaultproject.io/), and more.

## Conclusion

We hope this tool is as useful to others as it has been to us, allowing you deploy with Nix, without needing root and a the full system configuration to deploy a full NixOS profile.

Feel free to check out [`deploy-rs` on GitHub](https://github.com/serokell/deploy-rs) and leave us a Star if you like our project.
