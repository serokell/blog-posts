# Website deployment for Runtime Verification

In this blog post, we will provide insights into the website deployment workflow developed during our collaboration with [Runtime Verification](https://runtimeverification.com/).

Our collaboration focused on creating nix-based deployment workflow for small websites associated with Runtime Verification projects, including:
  - [Kontrol](https://kontrol.runtimeverification.com/): developer tooling for formal verification of smart contracts written in solodity.
  - [ERCx](https://ercx.runtimeverification.com/): developer tooling for ERC token testing.
  - [Firefly](https://fireflyblockchain.com/): developer tooling for Etherium smart contract testing.

We will walk through a simple example that showcases all the key features of this deployment workflow:
  - Deployment of `master` and `develop` branches using `deploy-rs`.
  - Secure deployment of secrets using `deploy-rs` and `sops-nix`.
  - Deployment of separate website instances on each open pull request to simplify the review process.

## Nixify website
To begin, we must create nix expressions so that we can build our website using nix. Because different websites use different technologies, these expressions can differ significantly. In this blog post, we will look at deployment of a website created using the following command:

```
yarn create next-app --example hello-world hello-world-app
```

To package our website, we need to create a `flake.nix` file in root of our repository with the following content:

<details>
  <summary> <b>Click to expand flake.nix</b> </summary>

```nix
{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nix-npm.url = "github:serokell/nix-npm-buildpackage";
  };

  outputs = { self, nixpkgs, nix-npm, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          nix-npm.overlays.default
          (self: super: { nodejs = super.nodejs-18_x; })
        ];
      };

      hello-world-app-package = pkgs.buildYarnPackage {
        name = "hello-world-app-package";
        src = builtins.path { name = "hello-world-app"; path = ./hello-world-app; };
        postInstall = ''
          mv ./.next $out/
        '';
        yarnBuild = ''
          yarn install
          yarn build
        '';
      };

      hello-world-app = pkgs.writeShellApplication {
        name = "hello-world-app";
        text = ''
          ${hello-world-app-package}/bin/yarn start -p "$PORT" ${hello-world-app-package}
          '';
        };
    in {
      packages.x86_64-linux = { inherit hello-world-app; };
    };
}
```

</details>

We use [`nix-npm-buildpackage`](https://github.com/serokell/nix-npm-buildpackage) to build our website. First, we introduce the `buildYarnPackage` function into `nixpkgs` by applying an overlay from `nix-npm-buildpackage`. Additionally, we overlay the `nodejs` package to specify a particular version:

```nix
pkgs = import nixpkgs {
  inherit system;
    overlays = [
      nix-npm.overlays.default
      (self: super: { nodejs = super.nodejs-18_x; })
    ];
};

```
More information about overlays can be found in [NixOS wiki](https://nixos.wiki/wiki/Overlays).

Next, we utilize `buildYarnPackage` to download the website dependencies with nix and build the package:
```nix
hello-world-app-package = pkgs.buildYarnPackage {
  name = "hello-world-app-package";
  src = builtins.path { name = "hello-world-app"; path = ./hello-world-app; };
  postInstall = ''
    mv ./.next $out/
  '';
  yarnBuild = ''
    yarn install
    yarn build
  '';
};
```
Afterward, we can define the website's executable using `hello-world-app-package`. To achieve this, we simply utilize the `yarn` executable provided by `hello-world-app-package` to start the server with the `start` command defined in `package.json`:

```nix
hello-world-app = pkgs.writeShellApplication {
  name = "hello-world-app";
  text = ''
    ${hello-world-app-package}/bin/yarn start -p "$PORT" ${hello-world-app-package}
  '';
};
```
The `hello-world-app` is available as `packages` flake output, in order to build it run:
```
nix build .#hello-world-app
```

We can also use `hello-world-app-package` to define checks for our project, such as the `prettier` check:
```nix
mkYarnCheck = name: command:
  pkgs.runCommand "hello-world-app-${name}" {} ''
    ${hello-world-app-package}/bin/yarn run ${command}
    touch $out
'';

prettier-check = mkYarnCheck "prettier" "prettier --check \"pages/**/*.tsx\"";
```

In `mkYarnCheck` function, we once again use `yarn` to execute the `prettier` check within the nix build. Note that since the check doesn't produce any output, we create a blank output using `touch $out`.

If you want to learn more about flakes, check out our [blogpost about flakes](https://serokell.io/blog/practical-nix-flakes) and related [NixOS wiki page](https://nixos.wiki/wiki/Flakes).

## NixOS module

After that we have nix package for our website next step would be providing nixos module that would define configuration options and systemd service for our website:

<details>
  <summary> <b>Click to expand module.nix</b> </summary>

```nix
{ self, ... }@ inputs:
{ config, pkgs, lib, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types mkMerge;
  cfg = config.services.hello-world-app;
in
{
  options.services.hello-world-app = {
    enable = lib.mkEnableOption "hello-world-app";
    package = mkOption {
      type = types.path;
      default = self.packages.x86_64-linux.hello-world-app;
      description = ''
        Path to the package
      '';
    };
    port = mkOption {
      type = types.port;
      default = 3000;
      description = ''
        Port of the site
      '';
    };
    secretsFile = mkOption {
      type = types.path;
      description = ''
        Path to the secrets file on the deployment server
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.hello-world-app = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig.ConditionPathExists = [
        "${cfg.package}/bin/hello-world-app"
        cfg.secretsFile
      ];
      serviceConfig.EnvironmentFile = cfg.secretsFile;
      environment = {
        PORT = toString cfg.port;
      };
      script = ''
        ${cfg.package}/bin/hello-world-app
      '';
    };
  };
}
```

</details>

In the `options` section, we define settings for our website. In this example, we have:
  - `enable`: This option enables our systemd service.
  - `package`: It allows us to override the website package.
  - `port`: We can specify the port for our website.
  - `secretsFile`: This option specifies the path to the environment file with secrets located on the server.

Please note that we don't include secrets directly as options, instead, we provide the path to the environment file. This is because all nix expressions end up stored `/nix/store`, which is world-readable. We will cover secrets deployment later in this blog post.

In the `config` section, we specify the `systemd` service, which is added to the NixOS configuration if the `enable` option is set to `true`:

```nix
systemd.services.hello-world-app = {
  after = [ "network.target" ];
  wantedBy = [ "multi-user.target" ];
  unitConfig.ConditionPathExists = [
    "${cfg.package}/bin/hello-world-app"
    cfg.secretsFile
  ];
  serviceConfig.EnvironmentFile = cfg.secretsFile;
  environment = {
    PORT = toString cfg.port;
  };
  script = ''
    ${cfg.package}/bin/hello-world-app
  '';
};
```
We pass all non-secret environment variables via the `environment` attribute and secrets via `serviceConfig.EnvironmentFile`. We then start our website by executing it in the `script` attribute. We also specify `ConditionPathExists` to check whether our secrets file and executable exist before running the `script`.

To learn more about NixOS modules visit [NixOS wiki](https://nixos.wiki/wiki/NixOS_modules).

When our module is ready, we should add it to the `nixosModules` output of our flake like this:

```nix
nixosModules = { default = import ./module.nix inputs; };
```

You can also create a virtual machine using your freshly crafted module for local testing. This proves advantageous, particularly when your module contains multiple systemd units. For instance, you might have systemd units for frontend, backend, and one-shot service handling database migrations.

To create virtual machine we need to add extra output to our flake:

```nix
nixosConfigurations.hello-world-app = nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [ ({ config, pkgs, ... }: {
    virtualisation.vmVariant.virtualisation = {
      graphics = false;
      memorySize = 2048;
      diskSize = 4096;
      cores = 2;
    };
    networking.firewall.allowedTCPPorts = [ 3000 ];

    imports = [ self.nixosModules.default ];

    services.hello-world-app = {
      enable = true;
      port = 3000;
      secretsFile = "/etc/secrets.env";
    };

    users.users.root.password = "";

    environment.etc."secrets.env".text = ''
      SECRET_VARIABLE="SOME DEFAULT SECRET VALUE FOR DEVELOPMENT"
    '';
  })];
};
```
Here is a breakdown of the configuration:
  - `virtualisation.vmVariant.virtualisation` settings define the virtual machine's parameters such as memory, disk size, and number of CPU cores.
  - `networking.firewall.allowedTCPPorts` specifies the ports (`3000` in this case) to forward for accessing our website.
  - `imports` attribute imports your previously defined module.
  - `services.hello-world-app` configures your module.
  - `users.users.root.password` sets the root user password to an empty string, allowing login without a password.
  - `environment.etc."secrets.env".text` creates a secrets file with some default values for local development.

To build and run the virtual machine, follow these commands:
```bash
# Build the VM
nix build .#nixosConfigurations.hello-world-app.config.system.build.vm
# Run the VM, forwarding port 3000
sudo QEMU_NET_OPTS='hostfwd=tcp::3000-:3000' result/bin/run-nixos-vm
```
After executing these commands, your website should be accessible in a web browser at [http://localhost:3000/](http://localhost:3000/). You can also log in to the VM as `root` with no password.

## Deployment of master and develop branches

Now that we have module for our website we can begin the deployment process. We will deploy our website service as nix profile under a non-priveleged user. To accomplish this, we are going to use [deploy-rs](https://github.com/serokell/deploy-rs), a nix flake deployment tool designed to deploy nix profiles. We have [blog post](https://serokell.io/blog/deploy-rs) about `deploy-rs`.

### Server configuration
For website deployment we will need a server, we are going to use NixOS server hosted on AWS. Please note that it is not necessary to use NixOS on the server, only nix will be enough to deploy the profile using `deploy-rs`. 

The server configuration looks like this:

<details>
  <summary> <b>Click to expand hello-world-server.nix</b> </summary>

```nix
{ modulesPath, pkgs, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  users = {
    users = {
      root.openssh.authorizedKeys.keys = [ "<PUBLIC_SSH_KEY_FOR_ROOT>" ];
      hello-world = {
        group = "hello-world";
        home = "/home/hello-world";
        createHome = true;
        isNormalUser = true;
        useDefaultShell = true;
        extraGroups = [ "systemd-journal" ];
        openssh.authorizedKeys.keys = [ "<PUBLIC_SSH_KEY_FOR_DEPLOYMENT>" ];
      };
    };
    groups.hello-world = {};
  };

  nix.settings.trusted-users = [ "hello-world" ];

  # Enable lingering for hello-world user
  systemd.tmpfiles.rules = [
    "f /var/lib/systemd/linger/hello-world 644 root root"
  ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = let
    mkStaticHost = {port, extraConfig}: pkgs.lib.recursiveUpdate {
      forceSSL = true;
      locations = {
        "/".proxyPass = "http://127.0.0.1:${toString port}";
      };
    } extraConfig;
  in {
    enable = true;
    virtualHosts."hello-world-app.com" = mkStaticHost {
      port = 3000;
      extraConfig.enableACME = true;
    };
    virtualHosts."sandbox.hello-world-app.com" = mkStaticHost {
      port = 3001;
      extraConfig.enableACME = true;
    };
  };
  security.acme = {
    defaults.email = "contact@hello-world-app.com";
    acceptTerms = true;
  };

  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 1d";
  };

  system.stateVersion = "23.05";
}
```
</details>

Server configuration will also be deployed with `deploy-rs`. First, we need to set public SSH key for the `root` or any other priveleged user so that we can deploy server configuration with `deploy-rs`:
```nix
root.openssh.authorizedKeys.keys = [ "<PUBLIC_SSH_KEY_FOR_ROOT>" ];
```
We also need a non-previledged user under which we will deploy and run our services:
```nix
hello-world = {
  group = "hello-world";
  home = "/home/hello-world";
  createHome = true;
  isNormalUser = true;
  useDefaultShell = true;
  extraGroups = [ "systemd-journal" ];
  openssh.authorizedKeys.keys = [ "<PUBLIC_SSH_KEY_FOR_DEPLOYMENT>" ];
};
```
It is important to set `isNormalUser` to `true` and add user to the `systemd-journal` group so that we can check logs with `journalctl`. Additionally, we need to add public SSH key that will be used for website deployment from CI.

We should also add our deployment user to nix `trusted-users` so that we can copy to nix store:
```nix
nix.settings.trusted-users = [ "hello-world" ];
```

Since our systemd services will be deployed as user-level services, they will not start automatically on startup unless we enable lingering for our user. To do this, we need to create a file `/var/lib/systemd/linger/<user_name>`:

```nix
systemd.tmpfiles.rules = [
  "f /var/lib/systemd/linger/hello-world 644 root root"
];
```
Finally, we need to setup `nginx`, TLS certificate generation and open HTTP and HTTPS ports:

```nix
services.nginx = let
  mkHost = {port, extraConfig}: pkgs.lib.recursiveUpdate {
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString port}";
    };
  } extraConfig;
  in {
    enable = true;
    virtualHosts."hello-world-app.com" = mkHost {
      port = 3000;
      extraConfig.enableACME = true;
    };
    virtualHosts."sandbox.hello-world-app.com" = mkHost {
      port = 3001;
      extraConfig.enableACME = true;
    };
  };
security.acme = {
  defaults.email = "contact@hello-world-app.com";
  acceptTerms = true;
};
networking.firewall.allowedTCPPorts = [ 80 443 ];
```
The nginx configuration passes all requests to `hello-world-app.com` to a service running on port `3000` (website deployed from the `master` branch) and similarly for `sandbox.hello-world-app.com` and `3001` (deployed from the `develop` branch).

We also need to setup automatic garbage collection for our server. This setup triggers daily garbage collection, deleting items older than one day:

```nix
nix.gc = {
  automatic = true;
  dates = "daily";
  options = "--delete-older-than 1d";
};
```

Now, we need to create `flake.nix` with `deploy` output:

```nix
{
  inputs = {
    deploy-rs.url = "github:serokell/deploy-rs";
    nixpkgs.url = "nixpkgs/nixos-22.11";
  };

  outputs = {self, nixpkgs, flake-utils, deploy-rs, ...}@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      nixosConfigurations.hello-world-server = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./servers/hello-world-server.nix
        ];
      };
      deploy = {
        magicRollback = true;
        autoRollback = true;
        nodes.hello-world-server = {
          hostname = "hello-world-app.com";
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.hello-world-server;
          };
        };
      };

      checks = deploy-rs.lib.${system}.deployChecks self.deploy;

      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = [
          deploy-rs.packages.${system}.deploy-rs
        ];
      };
    };
}
```
We define `nixosConfigurations.hello-world-server` output by specifying previously defiled server configuration in the `modules` attribute.

In deploy output we need to define a node named `hello-world-server`. Node can have multiple profiles, but in our case, we only deploy the profile `system`. Activation script for the `system` profile is generated with `deploy-rs.lib.${system}.activate.nixos` from the previously defined server configuration.

Additionally, we include `deploy-rs.packages.${system}.deploy-rs` in `devShell`. And now we  can deploy our server configuration with:

```
nix develop -c deploy .#hello-world-server
```
We can use the `--ssh-user <user_name>` option to override the user used for connecting to our server via SSH, or customize SSH options with `--ssh-opts`. Alternatively, these settings can also be configured within node or profile configurations of deploy output. For more detailed information on configuring `deploy-rs`, please refer the `README.md` file in the `deploy-rs` [GitHub repository](https://github.com/serokell/deploy-rs).

In addition, `deploy-rs` provides checks that allow you to verify the success of your profile build before initiating the deployment. These checks can be generated using `deploy-rs.lib.${system}.deployChecks` by providing the `deploy` output as argument.

### Website deployment

With our server prepared for the deployment, we can now proceed with website deployment. To achieve, this we will create a file named `services.nix` where we will use a previously defined NixOS module to build deployment profile:

<details>
  <summary> <b>Click to expand services.nix</b> </summary>

```nix
{ nixpkgs, pkgs, modules, inputs, ... }:
{
  port ? 3000,
  branch ? "master",
  secretsFolder ? ./secrets,
  secretsPath ? "/home/hello-world-app/.config/hello-world-app/environment",
  sopsKey ? "/home/hello-world-app/.config/hello-world-app/keys.txt",
}: let
  inherit (pkgs.lib) concatStringsSep;

  serviceSuffix = "-${branch}";
  secrets = "${secretsFolder}/secrets${serviceSuffix}.env";

  secretsPathWithSuffix = "${secretsPath}${serviceSuffix}";

  nixosSystem = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = modules ++ [{
      services.hello-world-app = {
        enable = true;
        secretsFile = secretsPathWithSuffix;
        inherit port;
      };
      systemd.services.hello-world-app.preStart = ''
        while ! ( systemctl is-active network-online.target > /dev/null ); do sleep 1; done
      '';
      systemd.services.hello-world-app = {
        after = [ "sops-nix${serviceSuffix}.service" ];
        bindsTo = [ "sops-nix${serviceSuffix}.service" ];
      };
    }];
  };

  homeConfiguration = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      inputs.sops-nix.homeManagerModule
      {
        home = {
          username = "hello-world";
          homeDirectory = "/home/hello-world";
          stateVersion = "22.11";
        };
        sops = {
          defaultSopsFile = secrets;
          age.keyFile = sopsKey;
          defaultSopsFormat = "dotenv";
          defaultSymlinkPath = "%r/.secrets${serviceSuffix}";
          defaultSecretsMountPoint = "%r/.secrets${serviceSuffix}.d";
          secrets.hello-world-app-secrets.path = secretsPathWithSuffix;
        };
      }
    ];
  };

  serviceNames = [ "hello-world-app" "sops-nix" ];

  mkService = name: if name == "sops-nix" then sopsService
    else pkgs.writeText "${name}.service" nixosSystem.config.systemd.units."${name}.service".text;

  sopsExecStart = homeConfiguration.config.systemd.user.services.sops-nix.Service.ExecStart;
  sopsService = pkgs.writeText "$sops-nix.service" ''
    [Service]
    Type=oneshot
    ExecStart=${sopsExecStart}
    RemainAfterExit=true

    [Install]
    WantedBy=default.target
  '';

  copyServices = concatStringsSep "\n" (map (name: let serviceName = name + serviceSuffix; in ''
    rm -f -- "$HOME/.config/systemd/user/${serviceName}.service" "$HOME/.config/systemd/user/default.target.wants/${serviceName}.service"
    ln -s ${mkService name} "$HOME/.config/systemd/user/${serviceName}.service"
    ln -s "$HOME/.config/systemd/user/${serviceName}.service" "$HOME/.config/systemd/user/default.target.wants"
  '') serviceNames);

  activate = pkgs.writeShellScriptBin "activate" ''
    set -euo pipefail
    export XDG_RUNTIME_DIR="/run/user/$UID"
    mkdir -p "$HOME/.config/systemd/user" "$HOME/.config/systemd/user/default.target.wants"
    ${copyServices}
    systemctl --user daemon-reload
    systemctl --user restart ${concatStringsSep " " serviceNames}

    # Check if services started
    retry_count=0
    while [[ "$retry_count" -lt 5 ]]; do
      set +e
      ${isActive} > /dev/null && exit 0
      set -e
      retry_count=$((retry_count+1))
      sleep 5
    done
    # if services didn't start exit with failure
    exit 1
  '';
  isActive = concatStringsSep " > /dev/null && " (map (name: "systemctl --user is-active ${name}") serviceNames);

in pkgs.buildEnv {
  name = "hello-world-app-service";
  paths = [ activate ];
}
```

</details>

Here we define `nixosSystem` with our website systemd service. Unfortunately, user-level systemd services can't access `network.target` so `after = [ "network.target" ]` defined in our module does  not have any effect when service is deployed as user-level service. To address this issue, we add `preStart` script to our configuration:
```nix
systemd.services.hello-world-app.preStart = ''
  while ! ( systemctl is-active network-online.target > /dev/null ); do sleep 1; done
'';
```
This script ensures that our service won't start until `network.target` is active. This can be particularly useful when your services require a network connection, for instance, if they need to apply migrations to a remote database.

We also need to deploy website secrets, we chosen to use [`sops-nix`](https://github.com/Mic92/sops-nix) for this purpose. Unfortunately, the NixOS module provided in `sops-nix` requires `root` privileges to work and it is kind of hard to override this behavior. However, there is a [Home Manager module](https://github.com/nix-community/home-manager) that doesn't require `root` privileges, so we can use it to deploy secrets under the `hello-world` user.

Similar to our `nixosSystem` with the website service we define `homeConfiguration` with `sops-nix` service:

```nix
sops = {
  defaultSopsFile = secrets;
  age.keyFile = sopsKey;
  defaultSopsFormat = "dotenv";
  defaultSymlinkPath = "%r/.secrets${serviceSuffix}";
  defaultSecretsMountPoint = "%r/.secrets${serviceSuffix}.d";
  secrets.hello-world-app-secrets.path = secretsPathWithSuffix;
};
```
Here, `defaultSopsFile` is pointing to one of sops-encrypted env files, such as `secrets-master` or `secrets-develop` for the `master` and `develop` branches, respectively. It's mandatory to set `defaultSymlinkPath` and `defaultSecretsMountPoint` to some unique folders for each profile to avoid overwriting symlinks to this folder on each deployment.

To create these encrypted files first we need to create a `.sops.yaml`:
```yaml
keys:
  - &server age1fe4fk5wrfz952jzklu9fpmx2mq378t8hz0n47z6xrkmnjvyqtaqqg8y3dp
  - &ops AEF0 79C6 7618 8929 4124  22DB 3AB4 2DF3 F50E 3420
creation_rules:
  - path_regex: secrets/secrets-(master|develop).env$
    key_groups:
    - pgp:
      - *ops
      age:
      - *server
```
In the `keys` field, we define encryption keys that will be used to encrypt our secrets:
  - `server`: This is the public part of age key used by the server.
  - `ops`: This is GPG key used by someone responsible for managing secrets.

in `creation_rules`, we specify that we want to use our keys for encrypting all sops files that match the `path_regex`.

To create sops files, we use the `sops` CLI utility:
```
sops ./secrets/secrets-master.env
```
This will open `$EDITOR` with temporary file that will be encrypted when you close your editor. Here, we put all the secrets that will be used in our website service. For more information on how `sops` works, you can refer to the [documentation](https://github.com/getsops/sops).

Additionally, in our sops configuration, we have `age.keyFile` which is the path to the private age key, which should be present on server. This key will be used to decrypt sops file. We also specify `secrets.hello-world-app-secrets.path`, which points to path on the server where secrets will be stored after decryption.

To ensure that our service starts only after the secrets decryption process is finished, we include the `sops-nix` service in the `after` and `bindsTo` sections of website systemd unit configuration: 

```nix
systemd.services.hello-world-app = {
  after = [ "sops-nix${serviceSuffix}.service" ];
  bindsTo = [ "sops-nix${serviceSuffix}.service" ];
};
```
This configuration achieves two objectives:
  - The `after` section makes our website service wait until the `sops-nix` service finishes secrets decryption.
  - The `bindsTo` section ensures that our website service will fail if the secrets deployment carried out by `sops-nix` service fails.

Now that we have services for website and secrets deployment, our next step is to extract them from the `nixosSystem` and `homeConfiguration`. This extraction is performed in the `mkService` function:

```nix
mkService = name: if name == "sops-nix" then sopsService
    else pkgs.writeText "${name}.service" nixosSystem.config.systemd.units."${name}.service".text;
```
For the services defined in `nixosConfiguration`, we extract systemd service config as text. Due to the different structure in Home Manager's systemd service representation, for `sops-nix` we extract `execStart` script and define the service configuration:

```nix
sopsExecStart = homeConfiguration.config.systemd.user.services.sops-nix.Service.ExecStart;
sopsService = pkgs.writeText "$sops-nix.service" ''
  [Service]
  Type=oneshot
  ExecStart=${sopsExecStart}
  RemainAfterExit=true

  [Install]
  WantedBy=default.target
'';
```

The extracted services are used in the `copyServices`:

```nix
copyServices = concatStringsSep "\n" (map (name: let serviceName = name + serviceSuffix; in ''
  rm -f -- "$HOME/.config/systemd/user/${serviceName}.service" "$HOME/.config/systemd/user/default.target.wants/${serviceName}.service"
  ln -s ${mkService name} "$HOME/.config/systemd/user/${serviceName}.service"
  ln -s "$HOME/.config/systemd/user/${serviceName}.service" "$HOME/.config/systemd/user/default.target.wants"
'') serviceNames);
```
For each service in `serviceNames`, we first try to remove any existing configurations. Then, we create symlinks to our new service configurations in the systemd user services configuration folder, as well as in `default.target.wants`, ensuring our services start after user login or system start if lingering is enabled.

Finally, we can define our profile activation script:
```nix
activate = pkgs.writeShellScriptBin "activate" ''
  set -euo pipefail
  export XDG_RUNTIME_DIR="/run/user/$UID"
  mkdir -p "$HOME/.config/systemd/user" "$HOME/.config/systemd/user/default.target.wants"
  ${copyServices}
  systemctl --user daemon-reload
  systemctl --user restart ${concatStringsSep " " serviceNames}

  # Check if services started
  retry_count=0
  while [[ "$retry_count" -lt 5 ]]; do
    set +e
    ${isActive} > /dev/null && exit 0
    set -e
    retry_count=$((retry_count+1))
    sleep 5
  done
  # if services didn't start exit with failure
  exit 1
'';
isActive = concatStringsSep " > /dev/null && " (map (name: "systemctl --user is-active ${name}") serviceNames);
```
In this script, we begin by exporting `XDG_RUNTIME_DIR`, which is required for systemctl to function correctly. We create directories for our systemd service configurations if they are not present and copy our services. After copying services, we reload the systemd daemon with `systemctl --user daemon-reload` and then restart each service specified in `serviceNames` with `systemctl --user restart <services>`. Upon each redeployment, `.service` files for the respective profile are built and symlinked into the user's `HOME` directory and restarted afterwards.

We include a simple while loop that checks every 5 seconds for up to 30 seconds in total to determine whether all services have started successfully. If any of the deployed services fail to start, the deployment is aborted, otherwise the deployment is considered complete. In case of deployment failure, `deploy-rs` will execute activation script of the previous profile generation if it exists to perform rollback.

We need to make some adjustments to our `flake.nix` file. First, we include `deploy-rs`, `sops-nix`, and `home-manager` inputs. Then in the `let` block, we import `.services.nix`:
```nix
services = import ./services.nix {
  inherit (inputs) nixpkgs;
  inherit pkgs inputs;
  modules = [ nixosModules.default ];
};
```
We also add a helper function to create `deploy-rs` nodes:

```nix
mkNode = {hostname ? "hello-world-app.com", script ? "activate", branch, port}: {
  inherit hostname;
  sshUser = "hello-world";
  user = "hello-world";
  profiles = {
    "hello-world-${branch}".path = deploy-rs.lib.x86_64-linux.activate.custom (services { inherit branch port; }) "$PROFILE/bin/${script}";
  };
};
```

This node setup closely resembles the one in our server's flake, with the difference that we are using `hello-world` user instead of `root` and in the profile activation script, we use `deploy-rs.lib.x86_64-linux.activate.custom` which allows us to specify a custom activation script. In our case, we invoke `activate` script from our profile.

Now, let's add `deploy.nodes` for `master` and `develop` branches:

```nix
deploy = {
  nodes = {
    hello-world-master = mkNode {
      branch = "master";
      port = 3000;
    };
    hello-world-develop = mkNode {
      branch = "develop";
      port = 3001;
    };
  };
};
```
Additionally, we include `pkgs.sops`, `pkgs.age`, and `deploy-rs.packages.${system}.deploy-rs` to `devShell` and define `checks` flake output, which includes our `prettier-check` and `deploy-rs` checks:

```nix
checks.x86_64-linux = { inherit prettier-check; } // deploy-rs.lib.${system}.deployChecks self.outputs.deploy;
```

To set up our CI/CD pipeline for deploying our profiles, we use self-hosted GitHub runners with nix available.
Here is the CI configuration:

<details>
  <summary> <b>Click to expand main.yaml</b> </summary>

```yaml
name: hello-world-app CI/CD
on:
  pull_request:
  push:
    branches:
      - master
      - develop
jobs:
  check:
    runs-on: [self-hosted]
    steps:
      - uses: actions/checkout@v4
      - name: setup cachix
        uses: cachix/cachix-action@v12
        with:
          name: hello-world-cache
          authToken: ${{ secrets.CACHIX_TOKEN }}
      - name: build website
        run: nix build -L .#hello-world-app
      - name: check prettier
        run: nix build -L .#checks.x86_64-linux.prettier-check
        if: success() || failure()
      - name: check deploy schema
        run: nix build -L .#checks.x86_64-linux.deploy-schema
        if: success() || failure()
      - name: check deploy activate
        run: nix build -L .#checks.x86_64-linux.deploy-activate
        if: success() || failure()
  deploy-branch:
    needs: check
    if: ${{ github.ref == 'refs/heads/master' || github.ref == 'refs/heads/develop' }}
    runs-on: [self-hosted]
    steps:
      - uses: actions/checkout@v4
      - name: setup cachix
        uses: cachix/cachix-action@v12
        with:
          name: hello-world-cache
          authToken: ${{ secrets.CACHIX_TOKEN }}
      - uses: webfactory/ssh-agent@v0.7.0
        with:
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
      - name: deploy
        run: nix develop -c deploy .#hello-world-${GITHUB_REF##*/} --skip-checks
```

</details>

In this setup, there are two jobs: `check` for CI and `deploy-branch` for CD. In both of these jobs, we use `cachix/cachix-action` to cache nix builds. It's highly recommended to setup a nix cache for your CI.

The `check` job is the CI part where we build our website and  run all `checks` from the flake. The `deploy-branch` job is the CD part where we actually deploy the website on the pushes to the `master` and `develop` branches.

We utilize `webfactory/ssh-agent` to setup an SSH agent with the private key provided in `secrets.SSH_PRIVATE_KEY`. This private key should match the public one specified in `openssh.authorizedKeys.keys` for the `hello-world` user in the server configuration. Additionally, your server should be added to `known_hosts` on the server where you host GitHub runners. Alternatively, you can remove host checking with  `deploy.node.hello-world-<branch>.sshOpts = [ "-o" "StrictHostKeyChecking=no" ]`  or using the CLI option of `deploy-rs`: `--ssh-opts "-o StrictHostKeyChecking=no"`.

Finally, in the `deploy` step we deploy our node with:
```
nix develop -c deploy .#hello-world-<branch> --skip-checks
```
Now our website should be deployed and up. To check the status of our systemd service on the server we can connect via SSH and run:
```
sudo su hello-world
export XDG_RUNTIME_DIR="/run/user/$UID"
systemctl --user status hello-world-app-<master/develop>.service
```
We can also use `journalctl --user -feu hello-world-app-<master/develop>.service` to read service logs.

## Pull request deployment

[Runtime Verification](https://runtimeverification.com/) also wanted to deploy a website for each open pull request, in addition to those deployed from the `master` and `develop` branches, to simplify review. These websites should be accessible from a URL subdomain that contains the pull request number, such as `pr-123.sandbox.hello-world-app.com`, where `123` represents the GitHub pull request number. To accomplish this, a new deployment node is needed specifically for deploying websites from pull requests.

### Website deployment configuration

To define `hello-world-pr` deployment node we need to add some changes to `./service.nix`.

First we must add another sops file for pull request secrets deployment. To do this we make following changes:
```nix
secretsSuffix = if builtins.elem branch [ "master" "develop" ] then serviceSuffix else "-pr";
secrets = "${secretsFolder}/secrets${secretsSuffix}.env";
```
When deploying from from a pull request, our profile will use `./secrets/secrets-pr.env` sops file.

Additionally, we need a way to deactivate our deployments when pull requests are closed or merged. To accomplish this, we add a service deactivation script to the deployment profile. First we define a script to remove services:

```nix
removeServices = concatStringsSep "\n" (map (name: let serviceName = name + serviceSuffix; in ''
  set +e
  systemctl --user stop ${serviceName}.service
  systemctl --user disable ${serviceName}.service
  set -e
  rm -f -- "$HOME/.config/systemd/user/${serviceName}.service" "$HOME/.config/systemd/user/default.target.wants/${serviceName}.service"
  '') serviceNames);
```
This script stops and disables services when possible and remove their configuration files.

In the deactiavation script, we remove services, then reload the systemd daemon and reset failed services, finally removing decrypted secrets:

```nix
deactivate = pkgs.writeShellScriptBin "deactivate" ''
  set -euo pipefail
  export XDG_RUNTIME_DIR="/run/user/$UID"
  ${removeServices}
  systemctl --user daemon-reload
  systemctl --user reset-failed
  rm -f ${secretsPathWithSuffix}
'';
```

In `flake.nix`, a helper function `prNode` is defined:
```nix
prNumber = builtins.readFile ./pr-number;
prNode = script: mkNode {
  inherit script;
  branch = "pr${prNumber}";
  port = 43000 + pkgs.lib.mod (pkgs.lib.toInt prNumber) 1000;
};
```
This function accepts a script name to create nodes for the activation and deactivation of the deployment profile. For the `branch`, we simply use `"pr${prNumber}"`, and for the `port`, we use a range from `43000` to `43999`. To calculate port we take `prNumber` modulo `1000` and add this value to our initial port of `43000`. It is important to commit the `./pr-number` file with a placeholder value to enable the deployment checks to work and build the deployment profile.

Finally, we add two new nodes to the `deploy.nodes` output:

```nix
hello-world-pr = prNode "activate";
cleanup = prNode "deactivate";
```

`hello-world-pr` is used for deploying and activating services, and the `cleanup` node is used for deactivating our services. With these changes, we can proceed with the GitHub actions job for deployment.

### GitHub actions configuration

The Github actions configuration for pull request deployment closely resembles that of branch deployments:
```nix
deploy-pr:
  needs: check
  if: ${{ github.event_name == 'pull_request' && github.ref != 'refs/heads/develop' }}
  runs-on: [self-hosted]
  steps:
    - uses: actions/checkout@v4
    - name: setup cachix
      uses: cachix/cachix-action@v12
      with:
        name: hello-world-cache
        authToken: ${{ secrets.CACHIX_TOKEN }}
    - uses: webfactory/ssh-agent@v0.7.0
      with:
        ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
    - name: deploy
      run: |
        echo "${{ github.event.number }}" > pr-number
        nix develop -c deploy .#hello-world-pr --skip-checks
```
Two key differences to note: First, the trigger condition now deploys on pull requests created from branches other than `develop`. Second, in the deploy script we `echo` the pull request number to the `pr-number` file before deploying `.#hello-world-pr` node.

Additionally, it's possible to include GitHub actions configuration for deploying a deactivation script when a pull request is closed:

```nix
name: Cleanup

on:
  pull_request:
    types:
      - closed
jobs:
  cleanup:
    name: cleanup
    runs-on: [self-hosted]
    steps:
      - uses: actions/checkout@v3
      - name: setup cachix
        uses: cachix/cachix-action@v12
        with:
          name: hello-world-cache
          authToken: ${{ secrets.CACHIX_TOKEN }}
      - uses: webfactory/ssh-agent@v0.7.0
        with:
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
      - name: deploy ${{ inputs.branch }}
        run: nix develop -c deploy .#cleanup --skip-checks
```
However, this is not necessary as service deactivation will be handled on the server-side.

### Server configuration

First, we will need to set up SSL certificates for our subdomains. Since our subdomains are dynamically created we will use a wildcard cetrificate that can cover  any `*.sandbox.hello-world-app.com` subdomain. To aquire such a certificate, your DNS provider's API should support `DNS-01` challenge.


We will once again use `sops-nix` for secrets deployment, but this time we will use NixOS module instead of Home Manager module.

In `./servers/hello-world-server.nix` we add:

```nix
sops = {
  defaultSopsFile = ../secrets/secrets.yaml;
  age.keyFile = "/var/lib/sops-nix/key.txt";
};
```
As before, private age key should be present at `age.keyFile`.

Now, let's add our token to `.secrets/secrets.yaml`, so the unencrypted file would look like this:

```yaml
dns-provider-token: "<TOKEN>"
```
Then, we can create a file with `<TOKEN>` on our server by adding the following to the configuration:
```nix
sops.secrets.dns-provider-token = {};
```
The path of the decrypted token file can be referenced as `config.sops.secrets.dns-provider-token.path`.

Now, we can add the wildcard certificate for `sandbox.hello-world-app.com` and all its subdomains to the ACME configuration:
```nix
security.acme = {
  defaults.email = "contact@hello-world-app.com";
  acceptTerms = true;
  certs."sandbox.hello-world-app.com" = {
    group = "nginx";
    dnsProvider = "godaddy";
    credentialsFile = config.sops.secrets.dns-provider-token.path;
    extraDomainNames = ["*.sandbox.hello-world-app.com"];
  };
};
```

The next step is to adjust nginx configuration for dynamically deloyed websites. First, we change `services.nginx.virtualHosts."sandbox.hello-world-app.com"` to use the certificate we just created. Instead of `extraConfig.enableACME`, we use `extraConfig.useACMEHost`:

```nix
virtualHosts."sandbox.hello-world-app.com" = mkHost {
  port = 3001;
  extraConfig.useACMEHost = "sandbox.hello-world-app.com";
};
```

Then, add configuration for dynamic deployments in `services.nginx.appendHttpConfig`:

```nix
appendHttpConfig = let
  cert = config.security.acme.certs."sandbox.hello-world-app.com".directory;
in ''
  server {
    listen 0.0.0.0:80;
    listen [::0]:80;
    server_name ~^(pr-([0-9]+)).sandbox.hello-world-app.com;
    location / {
      return 301 https://$host$request_uri;
    }
  }

  server {
    listen 0.0.0.0:443 http2 ssl;
    listen [::0]:443 http2 ssl;
    server_name ~^(pr-(?<pr_number>[0-9]+)).sandbox.hello-world-app.com;

    ssl_certificate ${cert}/fullchain.pem;
    ssl_certificate_key ${cert}/key.pem;
    ssl_trusted_certificate ${cert}/chain.pem;

    location / {
      set_by_lua_block $server_port {
        return 43000 + ngx.var.pr_number % 1000
      }
      proxy_pass http://127.0.0.1:$server_port;
    }
  }
'';
```

The first `server` block forces SSL by redirecting all HTTP requests to use HTTPS for any URLs that match the `^(pr-([0-9]+)).sandbox.hello-world-app.com` regex. In the second `server` block, which handles HTTPS connections, we configure our `ssl_certificate`:

```nix
ssl_certificate ${cert}/fullchain.pem;
ssl_certificate_key ${cert}/key.pem;
ssl_trusted_certificate ${cert}/chain.pem;
```

We capture the `pr_number` variable in our `^(pr-(?<pr_number>[0-9]+)).sandbox.hello-world-app.com` regex. To calculate the port based on `pr_number`, we use a code block provided by the `lua` module:

```nix
set_by_lua_block $server_port {
  return 43000 + ngx.var.pr_number % 1000
}
```
This follows the same logic as on the website part: we take pull request number modulo `1000` and add it to initial port of `43000` to calculate the `$server_port` variable which then used in the `proxy_pass`.

To use `set_by_lua_block`, we need to add the `lua` and `develkit` modules to `nginx`:
```nix
additionalModules = with pkgs.nginxModules; [ lua develkit ];
```
With these changes, dynamically deloyed website from pull request number `NNN` is now accessed on `pr-<NNN>.sandbox.hello-world-app.com`.

The final step is to clean up services and profiles when pull requests are merged or closed. To achieve this, we define module in `./modules/cleanup.nix`:

<details>
  <summary> <b>Click to expand cleanup.nix</b> </summary>

```nix
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types mapAttrs';
  cfg = config.services.cleanup;
  cleanupConfig = types.submodule ({config, ...}: {
    options = {
      repo = mkOption {
        type = types.str;
        description = ''
          Github repository
        '';
      };
      githubOrganization = mkOption {
        type = types.str;
        default = "runtimeverification";
      };
      user = mkOption {
        type = types.str;
        description = ''
          Name of the user used to deploy services
        '';
      };
      bootTimer = mkOption {
        type = types.str;
        default = config.timer;
        description = ''
          Defines a timer relative to when the machine was booted up
        '';
      };
      timer = mkOption {
        type = types.str;
        default = "15m";
        description = ''
          Defines a timer relative to when service was last activated
        '';
      };
    };
  });
in {
  options.services.cleanup = {
    enable = mkEnableOption "cleanup";
    profiles = mkOption {
      type = types.attrsOf cleanupConfig;
      description = ''
        Attribute sets with configuration for each project, where attribute name is the name of the profile used in deploy-rs, without suffix
      '';
    };
    tokenFile = mkOption {
      type = types.path;
      description = ''
        File with Github API token with read access to pull requests
      '';
    };
  };

  config = let
    mkTimer = profileName: {user, timer, bootTimer, ...}: {
      wantedBy = [ "timers.target" ];
      unitConfig.ConditionUser = user;
      timerConfig = {
        OnBootSec = bootTimer;
        OnUnitActiveSec = timer;
        Unit = "${profileName}-cleanup.service";
      };
    };
    mkService = profileName: {repo, githubOrganization, user, ...}: {
      script = ''
        set -euo pipefail

        OPEN_PRS=$((${pkgs.curl}/bin/curl -L \
          -H "Accept: application/vnd.github+json" \
          -H "Authorization: Bearer $TOKEN"\
          -H "X-GitHub-Api-Version: 2022-11-28" \
          https://api.github.com/repos/${githubOrganization}/${repo}/pulls?state=open&per_page=100) \
          | ${pkgs.jq}/bin/jq '.[] | .number')

        [[ ! -z "$OPEN_PRS" ]] && echo "Found open pull requests with numbers: $(echo $OPEN_PRS)"

        cd /nix/var/nix/profiles/per-user/${user}/

        for n in $(ls | grep -oP '(?<=${profileName}-pr)\d+' | sort -u); do
          if [[ ! $(echo $OPEN_PRS) =~ (^| )$n($| ) ]]; then
            echo "Removing services associated with pull request nubmer $n"
            source ${profileName}-pr$n/bin/deactivate
            echo "Removing nix profile associated with pull request nubmer $n"
            rm -f /nix/var/nix/profiles/per-user/${user}/${profileName}-pr$n{,-*}
          fi
        done
      '';

      unitConfig.ConditionPathExists = [ cfg.tokenFile ];
      serviceConfig = {
        Type = "oneshot";
        EnvironmentFile = cfg.tokenFile;
      };
    };
  in mkIf cfg.enable {
    systemd.user.timers = mapAttrs' (name: opts: {
      name = "${name}-cleanup";
      value = mkTimer name opts;
    }) cfg.profiles;
    systemd.user.services = mapAttrs' (name: opts: {
      name = "${name}-cleanup";
      value = mkService name opts;
    }) cfg.profiles;
  };
}
```

</details>

This module is relatively simple: for each profile in the configuration, we need to provide the repository name, the GitHub organization name that owns the repository, the username under which our services are deployed and started, time interval between triggering the cleanup script, and the token file containing GitHub Personal Access Token with read access to pull requests.

The cleanup script itself is straightforward: it queries the GitHub API to get list of open pull requests (set it the `OPEN_PRS` variable). In the user profile folder, it finds all pull request numbers `n`, for which profiles with names `${profileName}-pr<n>` exist. Then it checks if `n` is an element of `OPEN_PRS`. If not, it first calls the deactivation script with `source ${profileName}-pr$n/bin/deactivate` to stop and remove services deployed with this profile. Then, it simply removes the profile and all it's generations with `rm -f /nix/var/nix/profiles/per-user/${user}/${profileName}-pr$n{,-*}`. Stale nix store paths will be removed on the next scheduled garbage collector run. This script is triggered by systemd timer.

To configure the `cleanup` module in our server config:

```nix
sops.secrets.gh-token.owner = "hello-world";
services.cleanup = {
  enable = true;
  tokenFile = config.sops.secrets.gh-token.path;
  profiles.hello-world = {
    repo = "hello-world-app";
    user = "hewllo-world";
  };
};
```

First, we add `gh-token` secret (this will also require placing this secret in `./secrets/secrets.yaml`) and set its `owner` to the `hello-world` user. In the `profiles`, we define the `hello-world` profile, which should match the profile name without the suffix (i.e. `-pr<NNN>` part). Note that there can be some other websites deployed on the same server, so you can have multiple profiles in the `cleanup` configuration. Now, when a pull request is merged or closed, corresponding profile and services will be removed on the next cleanup script trigger.

Our website deployment configuration for open pull requests is now complete. You can find the complete configurations for the website and server described in this blog post [here](https://github.com/serokell/website-deployment-example).

## Conclusion

Through our collaboration with [Runtime Verification](https://runtimeverification.com/), we have developed a versatile website deployment workflow using nix, deploy-rs, and GitHub actions. This workflow covers deployment for static branches, encrypted secrets, and dynamic deployments for open pull requests. 

The key concept involves deploying websites as systemd units within a nix profile. Although our initial profile included just two services (one for the website itself and another for secrets decryption), this approach is highly scalable. You can easily expand it to deploy any number of services, whether it is a separate backend service, an one-shot service for database migration, or any other service you require. Additionally, this approach supports managing an arbitrary number of deployment instances, in this blogpost, we have used this feature to simplify the pull request review process.
