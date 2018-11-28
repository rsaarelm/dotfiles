setlocal softtabstop=2
setlocal textwidth=78
setlocal formatoptions+=t

" Fold on titles
setlocal foldexpr=getline(v:lnum)=~'\\C\\v^([A-Z][a-z0-9]+){2,}$'?'>1':1 foldmethod=expr

" Go to local file wiki definition of WikiWord with gd
map <buffer> gd yiw/^\<<C-f>pa\>$<cr>zo

" Open external file WikiWord.wiki or WikiWord.wiki.html with gf
setlocal suffixesadd=.wiki,.wiki.html

" Use backslash in LaTeX style abbrevs
setlocal iskeyword+=\

iabbrev <buffer> \Gamma Γ
iabbrev <buffer> \Delta Δ
iabbrev <buffer> \Theta Θ
iabbrev <buffer> \Pi Π
iabbrev <buffer> \Sigma Σ
iabbrev <buffer> \Phi Φ
iabbrev <buffer> \Psi Ψ
iabbrev <buffer> \Omega Ω
iabbrev <buffer> \alpha α
iabbrev <buffer> \beta β
iabbrev <buffer> \gamma γ
iabbrev <buffer> \delta δ
iabbrev <buffer> \epsilon ε
iabbrev <buffer> \eta η
iabbrev <buffer> \theta θ
iabbrev <buffer> \kappa κ
iabbrev <buffer> \lambda λ
iabbrev <buffer> \mu μ
iabbrev <buffer> \pi π
iabbrev <buffer> \rho ρ
iabbrev <buffer> \sigma σ
iabbrev <buffer> \tau τ
iabbrev <buffer> \phi φ
iabbrev <buffer> \psi ψ
iabbrev <buffer> \omega ω

iabbrev <buffer> \sqrt √
iabbrev <buffer> \partial ∂
iabbrev <buffer> \exists ∃
iabbrev <buffer> \forall ∀
iabbrev <buffer> \cup ∪
iabbrev <buffer> \cap ∩
iabbrev <buffer> \emptyset Ø
iabbrev <buffer> \in ∈
iabbrev <buffer> \subset ⊂
iabbrev <buffer> \sum ∑
iabbrev <buffer> \int ∫
iabbrev <buffer> \oint ∮
iabbrev <buffer> \infty ∞
iabbrev <buffer> \dot ∙
iabbrev <buffer> \wedge ∧
iabbrev <buffer> \vee ∨
iabbrev <buffer> \degree °
iabbrev <buffer> \circ ∘
