-------------------------------------------------------------------------------
-- Pulumi Files
-- Highlighting for Pulumi.yaml, Pulumi.*.yaml, and Pulumi YAML IaC files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local pulumi    = {}


-------------------------------------------------------------------------------
-- Settings

pulumi.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Comments (YAML style)
  highlight(0, 'pulumiComment',            { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'pulumiTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Top-Level Project Keys
  highlight(0, 'pulumiProjectKey',         { fg = colors.pink,       bg = 'NONE' })  -- Top-level keys
  highlight(0, 'pulumiName',               { fg = colors.pink,       bg = 'NONE' })  -- name
  highlight(0, 'pulumiRuntime',            { fg = colors.pink,       bg = 'NONE' })  -- runtime
  highlight(0, 'pulumiDescription',        { fg = colors.pink,       bg = 'NONE' })  -- description
  highlight(0, 'pulumiMain',               { fg = colors.pink,       bg = 'NONE' })  -- main
  highlight(0, 'pulumiTemplate',           { fg = colors.pink,       bg = 'NONE' })  -- template
  highlight(0, 'pulumiBackend',            { fg = colors.pink,       bg = 'NONE' })  -- backend
  highlight(0, 'pulumiOptions',            { fg = colors.pink,       bg = 'NONE' })  -- options

  -- Main Sections
  highlight(0, 'pulumiSection',            { fg = colors.pink,       bg = 'NONE' })  -- Main sections
  highlight(0, 'pulumiConfig',             { fg = colors.pink,       bg = 'NONE' })  -- config
  highlight(0, 'pulumiResources',          { fg = colors.pink,       bg = 'NONE' })  -- resources
  highlight(0, 'pulumiVariables',          { fg = colors.pink,       bg = 'NONE' })  -- variables
  highlight(0, 'pulumiOutputs',            { fg = colors.pink,       bg = 'NONE' })  -- outputs

  -- Runtime Values
  highlight(0, 'pulumiRuntimeValue',       { fg = colors.turquoise,  bg = 'NONE' })  -- Runtime names
  highlight(0, 'pulumiRuntimeYaml',        { fg = colors.turquoise,  bg = 'NONE' })  -- yaml
  highlight(0, 'pulumiRuntimeNodejs',      { fg = colors.turquoise,  bg = 'NONE' })  -- nodejs
  highlight(0, 'pulumiRuntimePython',      { fg = colors.turquoise,  bg = 'NONE' })  -- python
  highlight(0, 'pulumiRuntimeGo',          { fg = colors.turquoise,  bg = 'NONE' })  -- go
  highlight(0, 'pulumiRuntimeDotnet',      { fg = colors.turquoise,  bg = 'NONE' })  -- dotnet
  highlight(0, 'pulumiRuntimeJava',        { fg = colors.turquoise,  bg = 'NONE' })  -- java

  -- Resource Names (user-defined)
  highlight(0, 'pulumiResourceName',       { fg = colors.turquoise,  bg = 'NONE' })  -- Logical resource names

  -- Resource Type
  highlight(0, 'pulumiType',               { fg = colors.blue,       bg = 'NONE' })  -- type key
  highlight(0, 'pulumiResourceType',       { fg = colors.turquoise,  bg = 'NONE' })  -- provider:module:Resource

  -- Properties
  highlight(0, 'pulumiProperties',         { fg = colors.blue,       bg = 'NONE' })  -- properties key
  highlight(0, 'pulumiProperty',           { fg = colors.blue,       bg = 'NONE' })  -- Property names


  -----------------------------------------------------------------------------
  -- Resource Options

  highlight(0, 'pulumiResourceOptions',    { fg = colors.blue,       bg = 'NONE' })  -- options key
  highlight(0, 'pulumiOption',             { fg = colors.blue,       bg = 'NONE' })  -- Option keys

  -- Dependency Options
  highlight(0, 'pulumiDependsOn',          { fg = colors.blue,       bg = 'NONE' })  -- dependsOn
  highlight(0, 'pulumiParent',             { fg = colors.blue,       bg = 'NONE' })  -- parent

  -- Protection Options
  highlight(0, 'pulumiProtect',            { fg = colors.blue,       bg = 'NONE' })  -- protect
  highlight(0, 'pulumiRetainOnDelete',     { fg = colors.blue,       bg = 'NONE' })  -- retainOnDelete
  highlight(0, 'pulumiDeleteBeforeReplace',{ fg = colors.blue,       bg = 'NONE' })  -- deleteBeforeReplace

  -- Provider Options
  highlight(0, 'pulumiProvider',           { fg = colors.blue,       bg = 'NONE' })  -- provider
  highlight(0, 'pulumiProviders',          { fg = colors.blue,       bg = 'NONE' })  -- providers
  highlight(0, 'pulumiVersion',            { fg = colors.blue,       bg = 'NONE' })  -- version
  highlight(0, 'pulumiPluginDownloadURL',  { fg = colors.blue,       bg = 'NONE' })  -- pluginDownloadURL

  -- Change Options
  highlight(0, 'pulumiIgnoreChanges',      { fg = colors.blue,       bg = 'NONE' })  -- ignoreChanges
  highlight(0, 'pulumiReplaceOnChanges',   { fg = colors.blue,       bg = 'NONE' })  -- replaceOnChanges

  -- Other Options
  highlight(0, 'pulumiAliases',            { fg = colors.blue,       bg = 'NONE' })  -- aliases
  highlight(0, 'pulumiImport',             { fg = colors.blue,       bg = 'NONE' })  -- import
  highlight(0, 'pulumiCustomTimeouts',     { fg = colors.blue,       bg = 'NONE' })  -- customTimeouts
  highlight(0, 'pulumiAdditionalSecretOutputs', { fg = colors.blue,  bg = 'NONE' })  -- additionalSecretOutputs
  highlight(0, 'pulumiDefaultProvider',    { fg = colors.blue,       bg = 'NONE' })  -- defaultProvider


  -----------------------------------------------------------------------------
  -- Config Options

  highlight(0, 'pulumiConfigOption',       { fg = colors.blue,       bg = 'NONE' })  -- Config options
  highlight(0, 'pulumiConfigType',         { fg = colors.blue,       bg = 'NONE' })  -- type
  highlight(0, 'pulumiConfigDefault',      { fg = colors.blue,       bg = 'NONE' })  -- default
  highlight(0, 'pulumiConfigSecret',       { fg = colors.blue,       bg = 'NONE' })  -- secret

  -- Config Types
  highlight(0, 'pulumiConfigTypeValue',    { fg = colors.turquoise,  bg = 'NONE' })  -- Type values
  highlight(0, 'pulumiTypeString',         { fg = colors.turquoise,  bg = 'NONE' })  -- String
  highlight(0, 'pulumiTypeNumber',         { fg = colors.turquoise,  bg = 'NONE' })  -- Number
  highlight(0, 'pulumiTypeInteger',        { fg = colors.turquoise,  bg = 'NONE' })  -- Integer
  highlight(0, 'pulumiTypeBoolean',        { fg = colors.turquoise,  bg = 'NONE' })  -- Boolean
  highlight(0, 'pulumiTypeListString',     { fg = colors.turquoise,  bg = 'NONE' })  -- List<String>
  highlight(0, 'pulumiTypeListNumber',     { fg = colors.turquoise,  bg = 'NONE' })  -- List<Number>


  -----------------------------------------------------------------------------
  -- Built-in Functions (fn::*)

  highlight(0, 'pulumiFunction',           { fg = colors.orange,     bg = 'NONE' })  -- fn::* functions
  highlight(0, 'pulumiFnPrefix',           { fg = colors.pink,       bg = 'NONE' })  -- fn::

  -- String Functions
  highlight(0, 'pulumiFnToBase64',         { fg = colors.orange,     bg = 'NONE' })  -- fn::toBase64
  highlight(0, 'pulumiFnFromBase64',       { fg = colors.orange,     bg = 'NONE' })  -- fn::fromBase64
  highlight(0, 'pulumiFnToJSON',           { fg = colors.orange,     bg = 'NONE' })  -- fn::toJSON
  highlight(0, 'pulumiFnJoin',             { fg = colors.orange,     bg = 'NONE' })  -- fn::join
  highlight(0, 'pulumiFnSplit',            { fg = colors.orange,     bg = 'NONE' })  -- fn::split
  highlight(0, 'pulumiFnSelect',           { fg = colors.orange,     bg = 'NONE' })  -- fn::select

  -- Invoke Function
  highlight(0, 'pulumiFnInvoke',           { fg = colors.orange,     bg = 'NONE' })  -- fn::invoke
  highlight(0, 'pulumiInvokeFunction',     { fg = colors.blue,       bg = 'NONE' })  -- function key in invoke
  highlight(0, 'pulumiInvokeArguments',    { fg = colors.blue,       bg = 'NONE' })  -- arguments key
  highlight(0, 'pulumiInvokeReturn',       { fg = colors.blue,       bg = 'NONE' })  -- return key

  -- Asset Functions
  highlight(0, 'pulumiFnFileAsset',        { fg = colors.orange,     bg = 'NONE' })  -- fn::fileAsset
  highlight(0, 'pulumiFnStringAsset',      { fg = colors.orange,     bg = 'NONE' })  -- fn::stringAsset
  highlight(0, 'pulumiFnRemoteAsset',      { fg = colors.orange,     bg = 'NONE' })  -- fn::remoteAsset

  -- Archive Functions
  highlight(0, 'pulumiFnFileArchive',      { fg = colors.orange,     bg = 'NONE' })  -- fn::fileArchive
  highlight(0, 'pulumiFnRemoteArchive',    { fg = colors.orange,     bg = 'NONE' })  -- fn::remoteArchive
  highlight(0, 'pulumiFnAssetArchive',     { fg = colors.orange,     bg = 'NONE' })  -- fn::assetArchive

  -- Other Functions
  highlight(0, 'pulumiFnSecret',           { fg = colors.orange,     bg = 'NONE' })  -- fn::secret
  highlight(0, 'pulumiFnReadFile',         { fg = colors.orange,     bg = 'NONE' })  -- fn::readFile
  highlight(0, 'pulumiFnStackReference',   { fg = colors.orange,     bg = 'NONE' })  -- fn::stackReference


  -----------------------------------------------------------------------------
  -- Interpolation

  highlight(0, 'pulumiInterpolation',      { fg = colors.purple,     bg = 'NONE' })  -- ${...} content
  highlight(0, 'pulumiInterpDelim',        { fg = colors.pink,       bg = 'NONE' })  -- ${ and }
  highlight(0, 'pulumiInterpVariable',     { fg = colors.white,      bg = 'NONE' })  -- Variable in ${...}
  highlight(0, 'pulumiInterpProperty',     { fg = colors.blue,       bg = 'NONE' })  -- Property access in ${...}


  -----------------------------------------------------------------------------
  -- Built-in Variables

  highlight(0, 'pulumiBuiltinVar',         { fg = colors.purple,     bg = 'NONE' })  -- Built-in variables
  highlight(0, 'pulumiPulumiNamespace',    { fg = colors.purple,     bg = 'NONE' })  -- pulumi.*
  highlight(0, 'pulumiCwd',                { fg = colors.purple,     bg = 'NONE' })  -- pulumi.cwd
  highlight(0, 'pulumiOrganization',       { fg = colors.purple,     bg = 'NONE' })  -- pulumi.organization
  highlight(0, 'pulumiProject',            { fg = colors.purple,     bg = 'NONE' })  -- pulumi.project
  highlight(0, 'pulumiStack',              { fg = colors.purple,     bg = 'NONE' })  -- pulumi.stack


  -----------------------------------------------------------------------------
  -- Values

  -- Booleans
  highlight(0, 'pulumiBoolean',            { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, 'pulumiTrue',               { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'pulumiFalse',              { fg = colors.blue,       bg = 'NONE' })  -- false

  -- Null
  highlight(0, 'pulumiNull',               { fg = colors.blue,       bg = 'NONE' })  -- null, ~

  -- Numbers
  highlight(0, 'pulumiNumber',             { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'pulumiInteger',            { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'pulumiFloat',              { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Strings
  highlight(0, 'pulumiString',             { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, 'pulumiStringQuoted',       { fg = colors.redLight,   bg = 'NONE' })  -- "quoted"
  highlight(0, 'pulumiStringPlain',        { fg = colors.redLight,   bg = 'NONE' })  -- plain string
  highlight(0, 'pulumiStringBlock',        { fg = colors.redLight,   bg = 'NONE' })  -- | or > block

  -- Escape Sequences
  highlight(0, 'pulumiEscape',             { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, etc.


  -----------------------------------------------------------------------------
  -- Punctuation

  highlight(0, 'pulumiColon',              { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'pulumiDash',               { fg = colors.white,      bg = 'NONE' })  -- - (list item)
  highlight(0, 'pulumiComma',              { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'pulumiBrackets',           { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'pulumiBraces',             { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'pulumiDot',                { fg = colors.white,      bg = 'NONE' })  -- .


  -----------------------------------------------------------------------------
  -- Common Providers

  -- Provider Namespace
  highlight(0, 'pulumiProviderNS',         { fg = colors.turquoise,  bg = 'NONE' })  -- Provider namespace

  -- AWS Provider (pulumi:providers:aws, aws:*)
  highlight(0, 'pulumiAWS',                { fg = colors.turquoise,  bg = 'NONE' })  -- aws
  highlight(0, 'pulumiAWSResource',        { fg = colors.turquoise,  bg = 'NONE' })  -- aws:*:*

  -- AWS Services
  highlight(0, 'pulumiAWSS3',              { fg = colors.turquoise,  bg = 'NONE' })  -- aws:s3:*
  highlight(0, 'pulumiAWSEC2',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:ec2:*
  highlight(0, 'pulumiAWSLambda',          { fg = colors.turquoise,  bg = 'NONE' })  -- aws:lambda:*
  highlight(0, 'pulumiAWSIAM',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:iam:*
  highlight(0, 'pulumiAWSRDS',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:rds:*
  highlight(0, 'pulumiAWSDynamoDB',        { fg = colors.turquoise,  bg = 'NONE' })  -- aws:dynamodb:*
  highlight(0, 'pulumiAWSSNS',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:sns:*
  highlight(0, 'pulumiAWSSQS',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:sqs:*
  highlight(0, 'pulumiAWSECS',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:ecs:*
  highlight(0, 'pulumiAWSEKS',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:eks:*
  highlight(0, 'pulumiAWSAPIGateway',      { fg = colors.turquoise,  bg = 'NONE' })  -- aws:apigateway:*
  highlight(0, 'pulumiAWSCloudWatch',      { fg = colors.turquoise,  bg = 'NONE' })  -- aws:cloudwatch:*
  highlight(0, 'pulumiAWSRoute53',         { fg = colors.turquoise,  bg = 'NONE' })  -- aws:route53:*
  highlight(0, 'pulumiAWSVPC',             { fg = colors.turquoise,  bg = 'NONE' })  -- aws:ec2:Vpc

  -- Azure Provider (azure-native:*)
  highlight(0, 'pulumiAzure',              { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native
  highlight(0, 'pulumiAzureResource',      { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:*:*

  -- Azure Services
  highlight(0, 'pulumiAzureStorage',       { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:storage:*
  highlight(0, 'pulumiAzureCompute',       { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:compute:*
  highlight(0, 'pulumiAzureNetwork',       { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:network:*
  highlight(0, 'pulumiAzureWeb',           { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:web:*
  highlight(0, 'pulumiAzureContainerService', { fg = colors.turquoise, bg = 'NONE' })  -- azure-native:containerservice:*
  highlight(0, 'pulumiAzureFunctions',     { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:web:WebApp
  highlight(0, 'pulumiAzureSQL',           { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:sql:*
  highlight(0, 'pulumiAzureCosmosDB',      { fg = colors.turquoise,  bg = 'NONE' })  -- azure-native:documentdb:*

  -- GCP Provider (gcp:*)
  highlight(0, 'pulumiGCP',                { fg = colors.turquoise,  bg = 'NONE' })  -- gcp
  highlight(0, 'pulumiGCPResource',        { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:*:*

  -- GCP Services
  highlight(0, 'pulumiGCPStorage',         { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:storage:*
  highlight(0, 'pulumiGCPCompute',         { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:compute:*
  highlight(0, 'pulumiGCPContainer',       { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:container:*
  highlight(0, 'pulumiGCPCloudFunctions',  { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:cloudfunctions:*
  highlight(0, 'pulumiGCPCloudRun',        { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:cloudrun:*
  highlight(0, 'pulumiGCPBigQuery',        { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:bigquery:*
  highlight(0, 'pulumiGCPPubSub',          { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:pubsub:*
  highlight(0, 'pulumiGCPSQL',             { fg = colors.turquoise,  bg = 'NONE' })  -- gcp:sql:*

  -- Kubernetes Provider (kubernetes:*)
  highlight(0, 'pulumiKubernetes',         { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes
  highlight(0, 'pulumiK8sResource',        { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes:*:*

  -- Kubernetes Resources
  highlight(0, 'pulumiK8sCore',            { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes:core/v1:*
  highlight(0, 'pulumiK8sApps',            { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes:apps/v1:*
  highlight(0, 'pulumiK8sNetworking',      { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes:networking.k8s.io/v1:*
  highlight(0, 'pulumiK8sRBAC',            { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes:rbac.authorization.k8s.io/v1:*
  highlight(0, 'pulumiK8sDeployment',      { fg = colors.turquoise,  bg = 'NONE' })  -- Deployment
  highlight(0, 'pulumiK8sService',         { fg = colors.turquoise,  bg = 'NONE' })  -- Service
  highlight(0, 'pulumiK8sConfigMap',       { fg = colors.turquoise,  bg = 'NONE' })  -- ConfigMap
  highlight(0, 'pulumiK8sSecret',          { fg = colors.turquoise,  bg = 'NONE' })  -- Secret
  highlight(0, 'pulumiK8sIngress',         { fg = colors.turquoise,  bg = 'NONE' })  -- Ingress
  highlight(0, 'pulumiK8sNamespace',       { fg = colors.turquoise,  bg = 'NONE' })  -- Namespace
  highlight(0, 'pulumiK8sPod',             { fg = colors.turquoise,  bg = 'NONE' })  -- Pod
  highlight(0, 'pulumiK8sStatefulSet',     { fg = colors.turquoise,  bg = 'NONE' })  -- StatefulSet
  highlight(0, 'pulumiK8sDaemonSet',       { fg = colors.turquoise,  bg = 'NONE' })  -- DaemonSet

  -- Docker Provider (docker:*)
  highlight(0, 'pulumiDocker',             { fg = colors.turquoise,  bg = 'NONE' })  -- docker
  highlight(0, 'pulumiDockerResource',     { fg = colors.turquoise,  bg = 'NONE' })  -- docker:*:*
  highlight(0, 'pulumiDockerImage',        { fg = colors.turquoise,  bg = 'NONE' })  -- docker:index:Image
  highlight(0, 'pulumiDockerContainer',    { fg = colors.turquoise,  bg = 'NONE' })  -- docker:index:Container
  highlight(0, 'pulumiDockerNetwork',      { fg = colors.turquoise,  bg = 'NONE' })  -- docker:index:Network
  highlight(0, 'pulumiDockerVolume',       { fg = colors.turquoise,  bg = 'NONE' })  -- docker:index:Volume

  -- Random Provider
  highlight(0, 'pulumiRandom',             { fg = colors.turquoise,  bg = 'NONE' })  -- random
  highlight(0, 'pulumiRandomResource',     { fg = colors.turquoise,  bg = 'NONE' })  -- random:index:*
  highlight(0, 'pulumiRandomString',       { fg = colors.turquoise,  bg = 'NONE' })  -- RandomString
  highlight(0, 'pulumiRandomPassword',     { fg = colors.turquoise,  bg = 'NONE' })  -- RandomPassword
  highlight(0, 'pulumiRandomUuid',         { fg = colors.turquoise,  bg = 'NONE' })  -- RandomUuid

  -- Command Provider
  highlight(0, 'pulumiCommand',            { fg = colors.turquoise,  bg = 'NONE' })  -- command
  highlight(0, 'pulumiCommandLocal',       { fg = colors.turquoise,  bg = 'NONE' })  -- command:local:*
  highlight(0, 'pulumiCommandRemote',      { fg = colors.turquoise,  bg = 'NONE' })  -- command:remote:*

  -- Pulumi Provider (internal)
  highlight(0, 'pulumiProviderType',       { fg = colors.turquoise,  bg = 'NONE' })  -- pulumi:providers:*
  highlight(0, 'pulumiStackReference',     { fg = colors.turquoise,  bg = 'NONE' })  -- pulumi:pulumi:StackReference


  -----------------------------------------------------------------------------
  -- Common Resource Properties

  -- Tags
  highlight(0, 'pulumiTags',               { fg = colors.blue,       bg = 'NONE' })  -- tags
  highlight(0, 'pulumiTagKey',             { fg = colors.blue,       bg = 'NONE' })  -- Tag keys
  highlight(0, 'pulumiTagValue',           { fg = colors.redLight,   bg = 'NONE' })  -- Tag values

  -- Naming
  highlight(0, 'pulumiNameProp',           { fg = colors.blue,       bg = 'NONE' })  -- name property
  highlight(0, 'pulumiDescription',        { fg = colors.blue,       bg = 'NONE' })  -- description property

  -- Networking
  highlight(0, 'pulumiVpcId',              { fg = colors.blue,       bg = 'NONE' })  -- vpcId
  highlight(0, 'pulumiSubnetId',           { fg = colors.blue,       bg = 'NONE' })  -- subnetId
  highlight(0, 'pulumiSecurityGroupIds',   { fg = colors.blue,       bg = 'NONE' })  -- securityGroupIds
  highlight(0, 'pulumiCidrBlock',          { fg = colors.blue,       bg = 'NONE' })  -- cidrBlock

  -- Compute
  highlight(0, 'pulumiInstanceType',       { fg = colors.blue,       bg = 'NONE' })  -- instanceType
  highlight(0, 'pulumiAmi',                { fg = colors.blue,       bg = 'NONE' })  -- ami
  highlight(0, 'pulumiKeyName',            { fg = colors.blue,       bg = 'NONE' })  -- keyName

  -- Storage
  highlight(0, 'pulumiBucket',             { fg = colors.blue,       bg = 'NONE' })  -- bucket
  highlight(0, 'pulumiAcl',                { fg = colors.blue,       bg = 'NONE' })  -- acl

  -- IAM
  highlight(0, 'pulumiRole',               { fg = colors.blue,       bg = 'NONE' })  -- role
  highlight(0, 'pulumiPolicy',             { fg = colors.blue,       bg = 'NONE' })  -- policy
  highlight(0, 'pulumiAssumeRolePolicy',   { fg = colors.blue,       bg = 'NONE' })  -- assumeRolePolicy


  -----------------------------------------------------------------------------
  -- Treesitter Groups (extends YAML)

  -- Since Pulumi YAML is YAML, we add Pulumi-specific captures
  highlight(0, '@keyword.pulumi',              { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@function.pulumi',             { fg = colors.orange,     bg = 'NONE' })  -- fn::* functions
  highlight(0, '@function.builtin.pulumi',     { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, '@variable.pulumi',             { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.pulumi',     { fg = colors.purple,     bg = 'NONE' })  -- pulumi.* vars
  highlight(0, '@type.pulumi',                 { fg = colors.turquoise,  bg = 'NONE' })  -- Resource types
  highlight(0, '@property.pulumi',             { fg = colors.blue,       bg = 'NONE' })  -- Properties
  highlight(0, '@string.pulumi',               { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@number.pulumi',               { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@boolean.pulumi',              { fg = colors.blue,       bg = 'NONE' })  -- Booleans
  highlight(0, '@punctuation.special.pulumi',  { fg = colors.pink,       bg = 'NONE' })  -- ${ }


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens

  highlight(0, '@lsp.type.property.pulumi',    { fg = colors.blue,       bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.variable.pulumi',    { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.pulumi',    { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.pulumi',      { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.pulumi',      { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.pulumi',     { fg = colors.pink,       bg = 'NONE' })  -- Keywords


  -----------------------------------------------------------------------------
  -- Stack Configuration Files (Pulumi.*.yaml)

  highlight(0, 'pulumiStackConfig',        { fg = colors.pink,       bg = 'NONE' })  -- config section
  highlight(0, 'pulumiStackConfigKey',     { fg = colors.blue,       bg = 'NONE' })  -- Config keys
  highlight(0, 'pulumiStackConfigValue',   { fg = colors.redLight,   bg = 'NONE' })  -- Config values
  highlight(0, 'pulumiEncryptedValue',     { fg = colors.purple,     bg = 'NONE' })  -- Encrypted values
  highlight(0, 'pulumiSecretValue',        { fg = colors.purple,     bg = 'NONE' })  -- [secret] marker
  highlight(0, 'pulumiEncryptionSalt',     { fg = colors.gray,       bg = 'NONE' })  -- encryptionsalt


  -----------------------------------------------------------------------------
  -- Component Resources

  highlight(0, 'pulumiComponent',          { fg = colors.turquoise,  bg = 'NONE' })  -- Component resources
  highlight(0, 'pulumiComponentType',      { fg = colors.turquoise,  bg = 'NONE' })  -- pkg:index:Component


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'pulumiError',              { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'pulumiDeprecated',         { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
end

return pulumi
