{ pkgs, config, ... }:

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  environment.systemPackages = with pkgs; [ vim tmux git ];
}
