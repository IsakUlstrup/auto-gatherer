module WebGlRenderer exposing (viewWebGl)

import Engine.Particle exposing (Particle)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


viewWebGl : Int -> Int -> Int -> List (Particle a) -> Html msg
viewWebGl width height pixelRatio particles =
    WebGL.toHtml
        [ Html.Attributes.width (width * pixelRatio)
        , Html.Attributes.height (height * pixelRatio)
        , class "game"
        ]
        (List.map viewParticle particles)


viewParticle : Particle a -> Entity
viewParticle particle =
    WebGL.entity
        vertexShader
        fragmentShader
        (triangleFan particle.radius 50)
        { perspective = perspective particle.position.x particle.position.y }


perspective : Float -> Float -> Mat4
perspective x y =
    Mat4.mul
        (Mat4.makePerspective 65 1 0.01 800)
        (Mat4.makeLookAt (vec3 0 0 -800) (vec3 x y 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


point2D : Float -> Float -> Vec3
point2D x y =
    vec3 x y 0


triangleFan : Float -> Int -> Mesh Vertex
triangleFan r segments =
    let
        p x y =
            Vertex (point2D x y) (vec3 1 0 0)

        point : Int -> Vertex
        point i =
            p (r * cos (toFloat i * 2 * pi / toFloat segments)) (r * sin (toFloat i * 2 * pi / toFloat segments))
    in
    WebGL.triangleFan
        (p 0 0 :: (List.range 0 segments |> List.map point))



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
